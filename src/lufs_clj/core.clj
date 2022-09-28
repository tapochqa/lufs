(ns lufs-clj.core
  (:require [pink.io.sound-file :as sf]
            [diff-eq.core :refer [ring-read]]
            [clojure.math :as m])
  (:gen-class))

(defn rms [a]
  (let [a' (map #(* % %) a)]
    (/ (reduce + a') (count a'))))
(defn mean [a]
    (/ (reduce + a) (count a)))

(defn transgate [coll2 coll1]
  ; transfer gated blocks coll2 -> coll1
  (map
    (fn [a b] 
      (map
        #(if (nil? %2) nil %1) a b)) 
    coll1
    [coll2 coll2]))

(defn get-coeffs [f-type A w0 a fc rate G Q]
  ; shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
  (let [A+1 (inc A) A-1 (dec A)
        cosw0 (m/cos w0)
        √A (m/sqrt A) √A2⍺ (* 2 √A a)
        A+1cosw0 (* A+1 cosw0) A-1cosw0 (* A-1 cosw0)
        --+ (- A-1 A+1cosw0) +-+ (+ A-1 A+1cosw0) -++ (- A+1 A+1cosw0)
        f1 (+ A+1 A-1cosw0 (- √A2⍺))
        A* (fn [& args] (* A (reduce * args)))
        I-⍺ (- 1 a) A⍺ (* a A) -:⍺A (/ a A)]
  (case f-type
    :high-shelf 
    { :b0 (A* (+ A+1 A-1cosw0 √A2⍺))      :b1 (A* +-+ -2)               :b2 (A* f1)
      :a0 (+ A+1 (- A-1cosw0) √A2⍺)       :a1 (* 2 --+)                 :a2 (- A+1 A-1cosw0 √A2⍺)}

    :low-shelf
    { :b0 (A* (+ A+1 (- A-1cosw0) √A2⍺))  :b1 (A* --+)                  :b2 (A* -++)
      :a0 (+ A+1 A-1cosw0 √A2⍺)           :a1 (* +-+ (- 2) )            :a2 f1}
    
    :high-pass 
    { :b0 (/ (inc cosw0) 2)               :b1 (- (inc cosw0))           :b2 (/ (inc cosw0) 2)
      :a0 (inc a)                         :a1 (* -2 cosw0)              :a2 I-⍺}
    
    :low-pass
    { :b0 (/ (- 1 cosw0) 2)               :b1 (- 1 cosw0)               :b2 (/ (- 1 cosw0) 2)
      :a0 (inc a)                         :a1 (* -2 cosw0)              :a2 I-⍺} 
    
    :peaking
    { :b0 (inc A⍺)                        :b1 (* -2 cosw0)              :b2 (- 1 A⍺)
      :a0 (inc -:⍺A)                      :a1 (* -2 cosw0)              :a2 (- 1 -:⍺A)}
    
    :notch
    { :b0 1                               :b1 (* -2 cosw0)              :b2 1
      :a0 (inc a)                         :a1 (* -2 cosw0)              :a2 I-⍺}
    
    :high-shelf-DeMan
    (let [K (m/tan (/ (* m/PI fc) rate)) K2 (* K K)
          Vh (m/pow 10.0 (/ G 20.0))
          Vb (m/pow Vh 0.499666774155)
          VbK (* Vb K) Vbk:Q (/ VbK Q)
          a0_ (+ 1 (/ K Q) K2)
          -:a0_ #(/ % a0_)
          dup #(* % 2.0)
          ]
    { :b0 (/ (+ Vh K2 Vbk:Q) a0_)         :b1 (-> (- K2 Vh) dup -:a0_)  :b2 (-> Vh (- Vbk:Q) (+ K2) -:a0_)
      :a0 1.0                             :a1 (-> (dec K2) dup -:a0_)   :a2 (-> 1.0 (- (/ K Q)) (+ K2))})
    
    :high-pass-DeMan
    (let [K (-> m/PI (* fc) (/ rate) m/tan)
          K2 (* K K)
          f1 #(/ % (+ 1.0 (/ K Q) K2))
          ]
    { :b0 1.0                             :b1 -2.0                      :b2 1.0
      :a0 1.0                             :a1 (-> 2.0 (*(- K2 1.0)) f1) :a2 (-> 1.0 (- (/ K Q)) (+ K2) f1)}))))

(defn apply-filter
  [ coll
    &
    { :keys [G Q fc rate f-type pb-gain] 
      :or   {G 0 Q 0 fc 0 rate 44100 f-type :high-shelf pb-gain 0}}]
  (let [A (m/pow 10.0 (/ G 40.0))
        w0 (* m/PI (/ fc rate))
        a  (/ (m/sin w0) (* 2.0 Q))
        coeffs (get-coeffs f-type A w0 a fc rate G Q)
        ; macroexpanded from kunstmusic/diff-eq
        biquad-tdI (let 
                      [ x-1 (double-array 2) 
                        y-1 (double-array 2) 
                        x-2 (long-array 1) 
                        y-2 (long-array 1)] 
                      (fn [x {:keys [b0 b1 b2 a0 a1 a2]}] 
                        (let [y (+  (* (/ b0 a0) x) 
                                    (* (/ b1 a0) (ring-read x-1 (aget x-2 0) -1)) 
                                    (* (/ b2 a0) (ring-read x-1 (aget x-2 0) -2)) 
                                    (- (* (/ a1 a0) (ring-read y-1 (aget y-2 0) -1))) 
                                    (- (* (/ a2 a0) (ring-read y-1 (aget y-2 0) -2))))] 
                        (aset-double  x-1 (aget x-2 0) x) 
                        (aset-double  y-1 (aget y-2 0) y) 
                        (aset-long    x-2 0 (mod (inc (aget x-2 0)) 2)) 
                        (aset-long    y-2 0 (mod (inc (aget y-2 0)) 2)) y)))]
        (map #(biquad-tdI % coeffs) coll)))


(defn lufs [table rate]
  ; [[1 2 10 -3 -7 ...][1 3 4 -4 6 ...]]
  (let [filtered (->> table
                      (map #(apply-filter % 
                        { :f-type :high-shelf 
                          :G 4.0 
                          :Q (/ 1 (m/sqrt 2.0)) 
                          :fc 1500.0
                          :rate rate}))
                      (map #(apply-filter % 
                        { :f-type :high-pass 
                          :G 0.0 
                          :Q 0.5
                          :fc 38.0
                          :rate rate})))
        T_g 0.4 ; 400 ms block size
        overlap 0.75 
        Gamma_a -70.0 ; initial abs threshold
        quiet? #(< % Gamma_a)
        
        block-size (int (* rate T_g))
        overlap-size (int (* block-size overlap))

        ; slice on 400ms blocks with overlap and count RMS on every block
        blocks (as-> filtered f
                      (map #(partition block-size overlap-size %) f)
                      (map #(map rms %) f)) 
        ;d1 (println blocks)

        energy #(->> % m/log10 (* 10) (+ -0.691))
        tg-mean-e (fn [a b] (->> (transgate a b) 
                                    (map #(remove nil? %))
                                    (map mean)
                                    (reduce +)
                                    energy))

        ; count L+R energy on every block and nullize blocks below abs threshold
        J_g (as-> blocks t 
                (zipmap (first t) (last t))
                (map #(reduce + %) t)
                (map #(-> % energy) t)
                (map #(if (quiet? %) nil %) t))

        ; calculate 2nd relative threshiold
        Gamma_r (+ -10 (tg-mean-e J_g blocks))

        ; nullize below 2nd theshold blocks on L+R energy coll
        ; then drop 'em from original coll and calculate mean on every channel
        ; then calculate energy of sum
        lufs (tg-mean-e
                  (map #(if (> Gamma_r %) nil %) J_g)
                  blocks)]
  lufs))

(comment
  (def t (-> (sf/load-table "resources/test.wav") last last first))
  (def t' [t t])
  (lufs t' 44100))













