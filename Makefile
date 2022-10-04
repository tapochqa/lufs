NI_ARGS = \
	--initialize-at-build-time \
	--report-unsupported-elements-at-runtime \
	--no-fallback \
	-jar target/default+uberjar/lufs-clj-0.1.0-SNAPSHOT-standalone.jar \
	-H:+PrintClassInitialization \
	-H:ReflectionConfigurationFiles=reflection-config.json \
	-H:+ReportExceptionStackTraces \
	-H:Log=registerResource \
	-H:Name=./builds/lufs-clj-

build-native-macos:
	sudo native-image ${NI_ARGS}macos

test-timing:
	lein uberjar
	gtime java -jar target/default+uberjar/lufs-clj-0.1.0-SNAPSHOT-standalone.jar resources/test-short.wav 44100