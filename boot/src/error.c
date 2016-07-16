#include "genheader.h"

#include "error.h"

#if HEADER_ONLY

#define fatal_error(...) fatal_error_(__func__, __FILE__, __LINE__, __VA_ARGS__)
#define guarantee(p, ...) do{ if(!(p)){ fatal_error("guarantee failed `" #p "': " __VA_ARGS__); } }while(0)
#define guarantee_errno(p, f) guarantee(p, "%s: %s", f, strerror(errno))

#endif

#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>

#include "render.h"
#include "eval.h"
#include "utils.h"
#include "debug.h"

HEADER_DECLARE
void fatal_error_(const char* func, const char* file, int line, char* format, ...){
    va_list argptr;
    va_start(argptr, format);
    int res = fprintf(stderr, "fatal error in %s at %s:%d: ", func, file, line);
    guarantee_errno(res >= 0, "fprintf");
    res = vfprintf(stderr, format, argptr);
    guarantee_errno(res >= 0, "vfprintf");
    res = fprintf(stderr, "\n");
    guarantee_errno(res >= 0, "fprintf");
    va_end(argptr);
    if(current_eval_env && current_eval_env->query){
        trace_term("while evaluating", current_eval_env->query);
    }
    BREAKPOINT;
    exit(1);
    UNREACHABLE;
}
