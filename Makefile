LANGUAGE := NoImplicitPrelude RankNTypes

GHCFLAGS += $(patsubst %, -X%, $(LANGUAGE))
GHCFLAGS += -hidir build -odir build

theremin: | build
	ghc -o $@ --make Theremin.hs -main-is Theremin $(GHCFLAGS)

build:
	mkdir build
