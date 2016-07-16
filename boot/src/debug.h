#include <signal.h>

#define BREAKPOINT raise(SIGTRAP)
