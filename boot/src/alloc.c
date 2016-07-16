#include "genheader.h"

#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "debug.h"

EXPORT
void* system_alloc(size_t size){
    void* ret = malloc(size);
    if(!ret){
        fprintf(stderr, "fatal error: memory allocation failed: %s\n", strerror(errno));
        BREAKPOINT;
        exit(1);
    }
    return ret;
}

EXPORT
void* system_realloc(void* p, size_t size){
    void* ret = realloc(p, size);
    if(!ret){
        fprintf(stderr, "fatal error: memory allocation failed: %s\n", strerror(errno));
        BREAKPOINT;
        exit(1);
    }
    return ret;
}
