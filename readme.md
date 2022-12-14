Integrated LUFS Meter in Clojure. Implements EBU R 128 standard.

[![Clojars Project](https://img.shields.io/clojars/v/org.clojars.tapochqa/lufs.svg)](https://clojars.org/org.clojars.tapochqa/lufs)

```clojure
(ns lufsomer.core
    (:require [lufs.core :refer [lufs lufs*]]))



; Measure LUFS from file or filename

(lufs "audio.wav") ; -18.860580104601013



; Measure LUFS of provided sample arrays and sample-rate.
; All values in arrays must be doubles between -1 and 1.

(defn gen-data [len rate]
      (repeatedly
      	(* len rate)
      	#(-> 	(rand-int 2000)
      			(- 1000)
      			(/ 1000.0))))

(let [sr 44100 len 10]
    (lufs* [(gen-data len sr)
      		(gen-data len sr)]
      		sr)) ; 1.4325250705544224
```



For now it's relatively slow and works only with 2-channel stereo WAV.

Algorithm is copied from [csteinmetz1/pyloudnorm](https://github.com/csteinmetz1/pyloudnorm). 

WAV to double arrays converter copied from 
[kunstmusik/pink](https://github.com/kunstmusik/pink/blob/master/src/main/pink/io/sound_file.clj )
with a bugfix.