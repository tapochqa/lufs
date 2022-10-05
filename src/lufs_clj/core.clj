(ns lufs-clj.core
  (:require [clojure.math :as m]
    [lufs-clj.file :refer [load-table]]
    [lufs-clj.filter :refer [lufs-filters]]
    [hiphip.double :as dbl])
  (:gen-class))

(defn- transgate 
  "Transfer gated blocks coll2 -> coll1."
  [coll2 coll1]
  (map
    (fn [a b] 
      (map
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
    (map #(remove nil? %))
    (map double-array)
    (map #(dbl/amean %))
    (reduce +)
    energy))




(defn- lufs* [^doubles table ^long rate]
  (let [T_g 0.4 ; 400 ms block size
        overlap 0.75 
        Gamma_a -70.0 ; initial abs threshold
        quiet? #(< % Gamma_a)
        
        block-size (int (* rate T_g))
        overlap-size (int (* block-size overlap))

        filtered (map #(lufs-filters % rate) table)
        blocks (map #(rms-blocks % block-size overlap-size) filtered)

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

(defn -main [path]
  (println (lufs path)))






(comment
  
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

  (lufs-filters (-> nt :data first) (:sample-rate nt))
  (lufs "test/media/test.wav")


  (defn gen-data [len rate]
    (repeatedly (* len rate) #(-> (rand-int 2000) (- 1000) (/ 1000.0))))

  (let [sr 44100 len 10]
    (lufs (gen-data len sr) (gen-data len sr) sr))
    


  (lufs (->> aa first) 48000)
  (-> aa first first))
    

