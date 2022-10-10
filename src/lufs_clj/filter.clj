(ns lufs-clj.filter 
	(:require [clojure.math :as m]))

(defn get-coeffs 
  "shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html"
  [f-type A w0 a fc rate G Q]
  (let [A+1 (inc A) A-1 (dec A)
        cosw0 (m/cos w0)
        √A (m/sqrt A) √A2⍺ (* 2 √A a)
        A+1cosw0 (* A+1 cosw0) A-1cosw0 (* A-1 cosw0)
        f1 (+ A+1 A-1cosw0 (- √A2⍺))
        A* (fn [& args] (* A (reduce * args)))
        I-⍺ (- 1 a) A⍺ (* a A) -:⍺A (/ a A)]
  (case f-type
    :high-shelf 
    { :b0 (A* (+ A+1 A-1cosw0 √A2⍺))      :b1 (A* (+ A-1 A+1cosw0) -2)  :b2 (A* f1)
      :a0 (+ A+1 (- A-1cosw0) √A2⍺)       :a1 (* 2 (- A-1 A+1cosw0))    :a2 (- A+1 A-1cosw0 √A2⍺)}

    :low-shelf
    { :b0 (A* (+ A+1 (- A-1cosw0) √A2⍺))  :b1 (A* (- A-1 A+1cosw0))     :b2 (A* (- A+1 A+1cosw0))
      :a0 (+ A+1 A-1cosw0 √A2⍺)           :a1 (* (+ A-1 A+1cosw0) -2)   :a2 f1}
    
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

(defn biquad-tdI
  "Filters the collection with coeffs."
  ^doubles 
  [^doubles coll
    len
    {:keys 
      [ ^double a0 ^double a1 ^double a2 
        ^double b0 ^double b1 ^double b2]}]
  (let [res (double-array len)
  		  b0' (/ b0 a0) b1' (/ b1 a0) b2' (/ b2 a0)
  		                a1' (/ a1 a0) a2' (/ a2 a0)]
    
    (loop
        [ c coll 
          x-2 0.0 y-2 0.0 x-1 0.0 y-1 0.0
          i 0]
        (if c
          (let [x (first c)
                y
                (+
                  (* b0' x)
                  (* b1' x-1)
                  (* b2' x-2)
                	(* a1' y-1 -1)
                	(* a2' y-2 -1))]
            (aset-double res i y)
            (recur 
              (next c)
              x-1 y-1 x y
              (unchecked-inc i)))
          res))))


(defn apply-filter
  "Applies biquad filter with provided params to collections."
	^doubles
  [ coll
    len
    &
    { :keys [G Q fc rate f-type pb-gain] 
      :or   {G 4.0 Q 0 fc 0 rate 44100 f-type :high-shelf pb-gain 0}}]
  (let [A (m/pow 10.0 (/ G 40.0))
        w0 (* m/PI (/ fc rate))
        a  (/ (m/sin w0) (* 2.0 Q))
        coeffs (get-coeffs f-type A w0 a fc rate G Q)]
    (biquad-tdI coll len coeffs)
    #_coeffs
    ))


(defn kw-hs 
  "K-weighted high-shelf filter params."
  [rate] {:f-type :high-shelf 
          :G 4.0 
          :Q (/ 1 (m/sqrt 2.0)) 
          :fc 1500.0
          :rate rate})

(defn kw-hp
  "K-weighted high-pass filter params."
  [rate] {:f-type :high-pass 
          :G 0.0 
          :Q 0.5
          :fc 38.0
          :rate rate})

(defn lufs-filters
  "K-weighted high-shelf, then K-weighted high-pass"
  ^doubles [^doubles arr ^long rate len]
  (-> arr   (apply-filter ,,, len (kw-hs rate))
            (apply-filter ,,, len (kw-hp rate))))






