Integrated LUFS Meter in Clojure.

(lufs "audio.wav")
=> -18.860580104601013

For now it's relatively slow and processes 1 min of 44.1k audio for about 1.5 sec. Works only with WAV.

Algorithm is taken from github.com/csteinmetz1/pyloudnorm. 
WAV to double arrays converter copied from github.com/kunstmusik/pink/blob/master/src/main/pink/io/sound_file.clj with a bugfix.