(ns lufs-clj.core
  (:require [clojure.math :as m]
    [lufs-clj.file :refer [load-table]]
    [lufs-clj.filter :refer [lufs-filters biquad-tdI]]
    [hiphip.double :as dbl])
  (:gen-class))

(defn- transgate 
  "Transfer gated blocks coll2 -> coll1."
  [coll2 coll1]
  (pmap
    (fn [a b] 
      (pmap
        #(if (nil? %2) nil %1) a b)) 
    coll1
    [coll2 coll2]))

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

(defn- tg-mean-e [a b]
  (->>  (transgate a b)
    (pmap #(remove nil? %))
    (pmap double-array)
    (pmap #(dbl/amean %))
    (reduce +)
    energy))

(defn pmap-filters
  [coll sr]
  (as-> coll d
        (sequence (comp (sliding-array 507150 507150)) d)
        (doall (pmap #(lufs-filters % sr) d))
        (apply concat d)))


(defn- lufs* [^doubles table ^long rate]
  (let [T_g 0.4 ; 400 ms block size
        overlap 0.75
        Gamma_a -70.0 ; initial abs threshold
        quiet? #(< % Gamma_a)
        
        block-size (int (* rate T_g))
        overlap-size (int (* block-size overlap))
        
        len (count (first table))
        
        filtered (pmap #(lufs-filters % rate len) table)

        blocks (pmap #(rms-blocks % block-size overlap-size) filtered)

        ; count L+R energy on every block and nullize blocks below abs threshold
        J_g (as-> blocks t
              (zipmap (nth t 0) (nth t 1))
              (map #(as-> % ch 
                      (reduce + ch) 
                      (energy ch) 
                      (if (quiet? ch) nil ch)) t))

        ; calculate 2nd relative threshold
        
        Gamma_r (+ -10 (tg-mean-e J_g blocks))

        zag (map #(if (> Gamma_r (or % 0)) nil %) J_g)

        ; nullize below 2nd theshold blocks on L+R energy coll
        ; then drop 'em from original coll and calculate mean on every channel
        ; then calculate energy of sum
        lufs 
        (tg-mean-e
          zag
          blocks)]
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
  
  (def t (-> (load-table "resources/test-short.wav") last last first))

  
  (lufs "test/media/test-short.wav")

  #_(map #(reduce conj %))
  #_(map #(reduce concat %))

  (def t' [t t])
  (def t'' (mapv #(lufs-filters % 44100) t'))
  (def t''' (mapv #(rms-blocks % 17640 0.75) t''))
  (zipmap (first t''') (last t'''))
  (lufs (->> t second) 44100)

  
  (map rms (partition 2 1 [1.0 2.0 3.0 4.0 5.0]))
  (def nt (load-table "test/media/test.wav"))
  (->> nt :data first (take 10))
  (->> nt :data last (take 10))

  
  (biquad-tdI (-> nt :data first) 
    { :b0 0.9999981679829839,
      :b1 -1.9999963359659678,
      :b2 0.9999981679829839,
      :a0 1.0027070379825762,
      :a1 -1.9999926719319356,
      :a2 0.9972929620174238})
  
  (defn ppmap
    "Partitioned pmap, for grouping map ops together to make parallel
    overhead worthwhile"
    [grain-size f & colls]
    (apply concat
     (apply pmap
            (fn [& pgroups] (doall (apply map f pgroups)))
            (map (partial partition-all grain-size) colls))))
  
  (def l (count (-> nt :data first)))
  
  
  (lufs-filters (-> nt :data first) (:sample-rate nt))
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
    

