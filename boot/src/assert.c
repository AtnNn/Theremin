#include "genheader.h"

#include "assert.h"

#if HEADER_ONLY

#ifdef ISABLE_ASSERTS
#define ASSERT_CODE(...)
#define assert(...)
#else
#define ASSERT_CODE(...) __VA_ARGS__
#define assert(p, ...) do{ if(!(p)){ fatal_error("assert failed `" #p "': " __VA_ARGS__); } }while(0)
#endif

#endif

struct unused;
