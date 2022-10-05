NI_ARGS = \
	--initialize-at-build-time \
	--report-unsupported-elements-at-runtime \
	--no-fallback \
	-jar target/default+uberjar/lufs-clj.jar \
	-H:+PrintClassInitialization \
	-H:ReflectionConfigurationFiles=reflection-config.json \
	-H:+ReportExceptionStackTraces \
	-H:Log=registerResource \
	-H:Name=./builds/lufs-clj-
FIXTURE = test/media/test.wav

build-native-macos:
	lein uberjar
	sudo native-image ${NI_ARGS}macos

test-timing:
	lein uberjar
	gtime java -jar target/default+uberjar/lufs-clj.jar ${FIXTURE}

test-timing-native:
	gtime builds/lufs-clj-macos ${FIXTURE}

cp:
	git add .
	@read -p "Commit message: " m; \
	git commit -m "$$m"
	git push --all gh