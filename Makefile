CC ?= clang
CCFLAGS ?= -Wall -Werror -g -std=c99 -pedantic-errors
LDFLAGS ?=

poorlog: boot/poorlog.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm -rf poorlog 
