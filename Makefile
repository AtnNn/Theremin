CC := clang
CFLAGS ?= -Wall -Wextra -Werror -Wswitch-enum -Wswitch-default -g -std=c99 -pedantic-errors
# CFLAGS ?= -O3 -DISABLE_ASSERTS -DISABLE_DEBUG
LDFLAGS ?=

poorlog: boot/poorlog.c Makefile
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ $<

test:
	./boot/test/run

clean:
	rm -rf poorlog
