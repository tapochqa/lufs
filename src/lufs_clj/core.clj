(ns lufs-clj.core
  (:require [clojure.math :as m])
  (:import [java.io File ByteArrayOutputStream 
            FileOutputStream BufferedOutputStream RandomAccessFile]
           [java.nio ByteBuffer ByteOrder]
           [javax.sound.sampled AudioFormat
            AudioFormat$Encoding AudioInputStream AudioSystem]
           )
  (:gen-class))


(defn- locate-file
  [f]
  (cond 
    (instance? File f) f
    (string? f) (File. ^String f)))


(defn convert-to-double-arrays
  "Convert 16-bit, big endian audio samples to +-1.0 double samples"
  [^ByteArrayOutputStream baos ^long channels]
  (let [barray ^bytes (.toByteArray baos)
        chan-length ^long (/ (alength barray) (* channels 2)) ; assumes 16-bit/2-byte per sample
        bbuffer (ByteBuffer/wrap barray)
        out (if (= channels 1) 
              (double-array chan-length)
              (into-array 
                (for [_ (range channels)] (double-array chan-length))))]
    (loop [i 0]
      (if (< i ^:2 chan-length)
        (do
          (if (= channels 1) 
              (aset ^doubles out i (double (/ (.getShort bbuffer) 32768.0)))
              (loop [j 0]
                (if 
                  (< j channels)
                  (let [ _ (aset ^doubles (aget ^"[[D" out j) i (double (/ (.getShort bbuffer) 32768.0)))]
                    (recur (unchecked-inc j)))
                  out)))
          (recur (unchecked-inc i)))
        out))))

(defn load-table
  "Load given file or filename as sampled audio.  Returns a map with meta-information as
  well as audio split into discrete channels. Converts to doubles from source format. 
  Currently only works with 16-bit PCM_SIGNED wave files."
  [f]
  (let [af ^File (locate-file f)
        ain0 ^AudioInputStream (AudioSystem/getAudioInputStream af)
        src-format ^AudioFormat (.getFormat ain0)
        sr (.getSampleRate src-format)
        ain ^AudioInputStream
        (AudioSystem/getAudioInputStream
          (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                        (long sr) 
                        16 
                        (.getChannels src-format) 
                        4 
                        (long sr) 
                        true)
          ain0) 
        channels (.getChannels src-format)
        baos (ByteArrayOutputStream.)
        buffer (byte-array 4096)]
    (loop [cnt (.read ain buffer)]
      (if (> cnt 0)
        (do 
          (.write baos buffer 0 cnt)
          (recur (.read ain buffer)))
        { :channels channels
          :sample-rate (int sr)
          :data (convert-to-double-arrays baos channels) 
         }))))


(defn get-coeffs [f-type A w0 a fc rate G Q]
  ; shepazu.github.io/Audio-EQ-Cookbook/audio-eq-cookbook.html
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
  ^doubles 
  [^doubles coll 
    {:keys 
      [ ^double a0 
        ^double a1 
        ^double a2 
        ^double b0 
        ^double b1 
        ^double b2]}]
  (loop  
      [ c coll 
        x-2 0.0 y-2 0.0 x-1 0.0 y-1 0.0
        res []]
      (if c
        (let [x (first c)
              y
              (+
                (* (/ b0 a0) x) 
                (* (/ b1 a0) x-1)
                (* (/ b2 a0) x-2)
              (-(* (/ a1 a0) y-1))
              (-(* (/ a2 a0) y-2)))]
          (recur 
            (next c)
            x-1 y-1 x y
            (conj res y)))
        res)))

(defn apply-filter
  [ coll
    &
    { :keys [G Q fc rate f-type pb-gain] 
      :or   {G 4.0 Q 0 fc 0 rate 44100 f-type :high-shelf pb-gain 0}}]
  (let [A (m/pow 10.0 (/ G 40.0))
        w0 (* m/PI (/ fc rate))
        a  (/ (m/sin w0) (* 2.0 Q))
        coeffs (get-coeffs f-type A w0 a fc rate G Q)]
        (biquad-tdI coll coeffs)))


(defn kw-hs [rate] {:f-type :high-shelf 
                    :G 4.0 
                    :Q (/ 1 (m/sqrt 2.0)) 
                    :fc 1500.0
                    :rate rate})

(defn kw-hp [rate] {:f-type :high-pass 
                    :G 0.0 
                    :Q 0.5
                    :fc 38.0
                    :rate rate})

(defn transgate [coll2 coll1]
  ; transfer gated blocks coll2 -> coll1
  (map
    (fn [a b] 
      (map
        #(if (nil? %2) nil %1) a b)) 
    coll1
    [coll2 coll2]))

(defn lufs-filters [ch rate]
  (-> ch  (apply-filter (kw-hs rate)) 
          (apply-filter (kw-hp rate))))

(defn rms [arr]
  (loop [xs arr
       result 0.0]
  (if xs
    (let [x (first xs)]
      (recur (next xs) (+ result (* x x))))
    (/ result (count arr)))))

(defn mean [a]
    (/ (reduce + a) (count a)))

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
        (map mean)
        (reduce +)
        energy))

(defn lufs* [table rate]
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
  (lufs path))




















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

    (-main "resources/test-short.wav")

    (lufs (->> aa first) 48000)
    (-> aa first first)
    )



