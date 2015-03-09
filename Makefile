CXX = clang
CXXFLAGS = -Wall -g
LDFLAGS =

boot_c := $(shell find boot -name '*.c')
boot_o := $(patsubst boot/%.c, build/boot/%.o, $(boot_c))
boot_d := $(patsubst boot/%.c, build/dep/%.d, $(boot_c))

.PHONY: default
default: poorlog

.PHONY: clean
clean:
	rm -rf build poorlog 

build/boot/%.o: boot/%.c $(MAKEFILE_LIST) | build/boot/. build/dep/.
	$(CXX) $(CXXFLAGS) -c -o $@ $< -MP -MQ $@ -MMD -MF build/dep/$*.d

poorlog: $(boot_o)
	$(CXX) $(CXXFLAGS) $(LDFLAGS) -o $@ $^

%/.:
	@mkdir -p $@

-include $(boot_d)
