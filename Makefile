LANGUAGE := NoImplicitPrelude RankNTypes

GHCFLAGS += $(patsubst %, -X%, $(LANGUAGE))
GHCFLAGS += -hidir build -odir build

SOURCES = Theremin.hs $(shell find Theremin | egrep '\.(hs|hs-boot)')

theremin: $(SOURCES) | build
	ghc -o $@ --make Theremin.hs -main-is Theremin $(GHCFLAGS)

build:
	mkdir build

clean:
	rm -rf build theremin
