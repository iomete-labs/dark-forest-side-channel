
BUILD := _build/default/main.exe

BIN := dark-forest-side-channel

.PHONY: build clean

all: build

build: ${BIN}

${BIN}: ${BUILD}
	@cp ${BUILD} ${BIN}
	@chmod 0700 ${BIN}

${BUILD}: $(shell ls -1 *.ml)
	@dune build

clean:
	@rm -rf _build
