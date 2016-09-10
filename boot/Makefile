CC := clang
CFLAGS ?= -O0 -Wall -Wextra -Werror -Wswitch-enum -Wswitch-default -g -std=c99 -pedantic-errors
# CFLAGS ?= -O3 -DISABLE_ASSERTS -DISABLE_DEBUG
LDFLAGS ?=

override CFLAGS += -iquote build/headers -iquote src

.DELETE_ON_ERROR:

poorlog:
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $^

build/deps.mk: Makefile tools/gendeps.py | build/.
	tools/gendeps.py -deps-out $@ -deps-target poorlog src/poorlog.c -o build/obj/poorlog.o $(CFLAGS)

include build/deps.mk

build/headers/%.h: src/%.c tools/genheader.py | build/headers/.
	tools/genheader.py $< > $@

build/obj/%.o: src/%.c | build/obj/.
	$(CC) $(CFLAGS) -c -o $@ $<

.PHONY: test
test:
	./test/run

clean:
	rm -rf build poorlog

.PRECIOUS: %/.
%/.:
	mkdir -p $*

etags:
	ctags -e `find src -type f`
