(ns lufs-clj.core
  (:require [clojure.math :as m]
            [lufs-clj.file :refer [load-table]]
            [lufs-clj.filter :refer [lufs-filters]]
            [hiphip.double :as dbl])
  (:gen-class))

(defn transgate [coll2 coll1]
  ; transfer gated blocks coll2 -> coll1
  (map
    (fn [a b] 
      (map
        #(if (nil? %2) nil %1) a b)) 
    coll1
    [coll2 coll2]))

(defn rms [arr]
  (loop [xs arr
       result 0.0]
  (if xs
    (let [x (first xs)]
      (recur (next xs) (+ result (* x x))))
    (/ result (count arr)))))

(defn mean [a]
    (dbl/amean a))

(defn sliding-array
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

(defn rms-blocks [ch block overlap]
  (sequence (comp (sliding-array block overlap) (map rms)) ch))

(defn energy [n]
  (->> n m/log10 (* 10) (+ -0.691)))

(defn tg-mean-e [a b]
  (->>  (transgate a b)
        (map #(remove nil? %))
        (map double-array)
        (map mean)
        (reduce +)
        energy))




(defn lufs* [^doubles table ^long rate]
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

        ; calculate 2nd relative threshiold
        
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

(defn lufs [path]
  (let [table (load-table path)
        data (:data table)
        rate (:sample-rate table)] 
    (lufs* data (/ rate 2))))

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
    (lufs-filters (-> nt :data first) (:sample-rate nt))
    (lufs "test/media/test.wav"))

    (lufs (->> aa first) 48000)
    (-> aa first first)
    )



