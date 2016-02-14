CC ?= clang
CCFLAGS ?= -Wall -g -std=gnu99
LDFLAGS ?=

poorlog: boot/poorlog.c
	$(CC) $(CCFLAGS) $(LDFLAGS) -o $@ $^

clean:
	rm -rf poorlog 
