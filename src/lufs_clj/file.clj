(ns lufs-clj.file 
   (:import [java.io File ByteArrayOutputStream 
            FileOutputStream BufferedOutputStream RandomAccessFile]
           [java.nio ByteBuffer ByteOrder]
           [javax.sound.sampled AudioFormat
            AudioFormat$Encoding AudioInputStream AudioSystem]))


(defn- locate-file
  [f]
  (cond 
    (instance? File f) f
    (string? f) (File. ^String f)))


(defn- convert-to-double-arrays
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