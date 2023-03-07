(ns lufs-clj.core
  (:require [clojure.math :as m]
    [lufs-clj.file :refer [load-table]]
    [lufs-clj.filter :refer [lufs-filters biquad-tdI]]
    [hiphip.double :as dbl])
  (:gen-class))


(defn rms 
  "Root mean square of collection. 
  Written with loop to be not too slow."
  [arr]
  (loop [xs arr
         result 0.0]
    (if xs
      (let [x (first xs)]
        (recur (next xs) (+ result (* x x))))
      (/ result (count arr)))))

(defn- sliding-array
  "Faster partition"
  ([^long n ^long step]
   (fn [rf]
     (let [a (java.util.ArrayDeque. n)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (.add a input)
          (if (= n (.size a))
            (let [v (.toArray a)]
              ;; Remove `step` elements instead of clear
              (dotimes [_ step] (.removeFirst a))
              (rf result v))
            result)))))))


(defn- rms-blocks [ch block overlap]
  (sequence (comp (sliding-array block overlap) (map rms)) ch))


(defn energy
  "Calculates energy of n. Works as intended if n is between 0 and 1" 
  [n]
  (->> n m/log10 (* 10) (+ -0.691)))


(defn tg-mean-e
  [^doubles ch1 ^doubles ch2 ^doubles gated len]
  (let
    [ res-1 (double-array len)
      res-2 (double-array len)]
    (loop [c1 ch1
           c2 ch2
           gtd gated
           
           i 0]
      (if c1
        (let [s1 (first c1)
              s2 (first c2)
              g (first gtd)]
            
          (when (some? g) (aset-double res-1 i s1) 
                          (aset-double res-2 i s2))
            
          (recur 
            (next c1) 
            (next c2) 
            (next gtd)
            
            (unchecked-inc i)))
      
        (energy
          (unchecked-add
            (dbl/amean res-1)
            (dbl/amean res-2)))))))


(defn pmap-filters
  [coll sr l]
  (as-> coll d
        (sequence (comp (sliding-array 507150 507150)) d)
        (doall (pmap #(lufs-filters % sr l) d))
        (apply concat d)))


(defn quiet->nil 
  [a]
  (if (< a -70.0) nil a))


(defn process-blocks
  [^doubles ch1 ^doubles ch2] 
    (loop [c1 ch1
           c2 ch2
           i 0
           res []]
      (if c1
        (let [s1 (first c1)
              s2 (first c2)]
          (recur
            (next c1) 
            (next c2)
            (unchecked-inc i)
            (conj res (-> (+ s1 s2) energy quiet->nil))))
        res)))


(defn- lufs* [^doubles table ^long rate]
  (let [T_g 0.4 ; 400 ms block size
        overlap 0.75
        Gamma_a -70.0 ; initial abs threshold
        quiet? #(< % Gamma_a)
        
        len (count (first table))
        
        block-size (int (* rate T_g))
        overlap-size (int (* block-size overlap))
        
        filtered (pmap #(lufs-filters % rate len) table)

        blocks (map #(rms-blocks % block-size overlap-size) filtered)
        
        blocks-0 (nth blocks 0) 
        blocks-1 (nth blocks 1)
        
        len-2 (count blocks-0)
        
        J_g (process-blocks 
                      blocks-0 
                      blocks-1)
        
        ; calculate 2nd relative threshold
        
        Gamma_r (+ 
                  -10 
                  (tg-mean-e 
                    blocks-0
                    blocks-1
                    J_g
                    len-2))

        zag (map #(if (> Gamma_r (or % 0)) nil %) J_g)

        ; nullize below 2nd theshold blocks on L+R energy coll
        ; then drop 'em from original coll and calculate mean on every channel
        ; then calculate energy of sum
        
        lufs 
        (tg-mean-e
          blocks-0
          blocks-1
          zag
          len-2)]
  
    
    (shutdown-agents)
    lufs))

(defn lufs
  "Measures integrated LUFS of the stereo WAV file or double array channels & sample-rate."
  ([file]
    (let [table (load-table file)
          data (:data table)
          rate (:sample-rate table)] 
      (lufs* data (/ rate 2))))
  ([^doubles ch1 ^doubles ch2 rate]
    (lufs* [ch1 ch2] rate)))

(defn println-lufs [path]
  (println (lufs path)))


(defn -main [path]
  (time (println-lufs path)))


(comment
  
  (lufs "test/media/test-short.wav")
  
  (def t (-> (load-table "resources/test-short.wav") last last first))

  

  #_(map #(reduce conj %))
  #_(map #(reduce concat %))

  (def t' [t t])
  (def t'' (mapv #(lufs-filters % 44100) t'))
  (def t''' (mapv #(rms-blocks % 17640 0.75) t''))
  (zipmap (first t''') (last t'''))
  (lufs (->> t second) 44100)

  
  (map rms (partition 2 1 [1.0 2.0 3.0 4.0 5.0]))
  (def nt (load-table "test/media/test-short.wav"))
  
  
  (->> nt :data first (take 10))
  (->> nt :data last (take 10))

  
  (biquad-tdI (-> nt :data first) 
    l
    { :b0 0.9999981679829839,
      :b1 -1.9999963359659678,
      :b2 0.9999981679829839,
      :a0 1.0027070379825762,
      :a1 -1.9999926719319356,
      :a2 0.9972929620174238}
    )
  
  (def l (count (-> nt :data first)))
  
  
  (lufs-filters (-> nt :data first) (:sample-rate nt) l)
  (pmap-filters (-> nt :data first) (:sample-rate nt) l)
  (count (sequence (comp (sliding-array 507150 507150)) (-> nt :data first)))

  
  
  
  
 
  (defn mean [vec]
    (/ (reduce + vec) (-> vec count double)))
  
  
  (mean [1197 1054 1166 1380])
  (mean [3737 4213 4513 4037])
  (println "abobus")


  (defn gen-data 
    [len rate]
    (repeatedly (* len rate) #(-> (rand-int 2000) (- 1000) (/ 1000.0))))
  
  (defn loud-data
    [len rate]
    (->> [-0.99999 0.99999] (repeat (* len rate)) (apply concat)))
   
  (defn sinusoid
    [len rate & {:keys [A f] :or {A 1 f 440}}]
    (->> 
      (range (* len rate)) 
      (map 
        #(* A (m/sin 
              (* 
                 2 
                 m/PI 
                 f 
                 %))))))
  (last 
    (sinusoid 20 200))
  
  (m/sin (* 2 m/PI 440 1))
  (m/sin 2764.601535159018)
  (m/sin 30)

  (let 
    [sr   44100 
     len  100
     wave (sinusoid 1 10000 len sr)]
    wave
    #_(lufs wave wave sr))

  (do (nil nil))

  )
    

