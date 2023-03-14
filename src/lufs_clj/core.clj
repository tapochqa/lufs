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
        (do
          (energy
            (unchecked-add
              (dbl/amean (double-array (remove #(= 0.0 %) res-1)))
              (dbl/amean (double-array (remove #(= 0.0 %) res-2))))))))))


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


(defn- lufs* [^doubles table ^long rate ^double window]
  (let [T_g window
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
        
        Gamma_r (-
                  (tg-mean-e 
                    blocks-0
                    blocks-1
                    J_g
                    len-2)
                  10)

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
    {:integrated lufs
     :gate Gamma_r
     :window-max (->> J_g (remove nil?) (reduce max))
     :vector (remove nil? J_g)}))


(defn integrated
  ([file]
    (let [table (load-table file)
          data (:data table)
          rate (:sample-rate table)] 
    (:integrated (lufs* data (/ rate 2) 0.4))))
  ([^doubles ch1 ^doubles ch2 rate]
    (:integrated (lufs* [ch1 ch2] rate 0.4))))


(defn short-term
  ([file]
    (let [table (load-table file)
          data (:data table)
          rate (:sample-rate table)] 
    (:window-max (lufs* data (/ rate 2) 3.0))))
  ([^doubles ch1 ^doubles ch2 rate]
    (:window-max (lufs* [ch1 ch2] rate 3.0))))


(defn momentary
  ([file]
    (let [table (load-table file)
          data (:data table)
          rate (:sample-rate table)] 
    (:window-max (lufs* data (/ rate 2) 0.4))))
  ([^doubles ch1 ^doubles ch2 rate]
    (:window-max (lufs* [ch1 ch2] rate 0.4))))


(defn lra
  [file]
  (let [table
        (load-table file)
        
        lufs
        (lufs* (:data table) (:sample-rate table) 1.48065)
        
        freqs
        (:vector lufs)
        
        gate
        (- (:gate lufs) 20.0)
        
        gated 
        (remove #(< % gate) freqs)
        
        gated
        (sort gated)
        
        len
        (count gated)
        
        low
        (nth gated (m/round (inc (/ (* 10.0 (- len 1)) 100))))
        
        hi
        (nth gated (m/round (inc (/ (* 95.0 (- len 1)) 100))))]
    
    
    (- hi low)))


(defn println-lufs [path]
  (println (integrated path)))


(defn -main [path]
  (time (println-lufs path)))


(comment
  (integrated "test/media/test.wav"))
    

