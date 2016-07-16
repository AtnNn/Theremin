#include "genheader.h"

#include "debug.h"

#if HEADER_ONLY

#include <errno.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "gc.h"

#define BREAKPOINT raise(SIGTRAP)

#define D_PARSE DEBUG_IF(debug_parse)
#define D_EVAL DEBUG_IF(debug_eval)
#define D_GC DEBUG_IF(debug_gc)
#define D_HASHTABLE DEBUG_IF(debug_hashtable)
#define D_ATOM DEBUG_IF(debug_atom)
#define D_STRING DEBUG_IF(debug_string)
#define D_SANITY DEBUG_IF(debug_sanity)

#ifdef ISABLE_DEBUG
#define DEBUG_IF(_) if(0)
#else
#define DEBUG_IF(p) \
    for(bool _debug_if = (disable_gc(), true); \
        _debug_if && p && *debug_enabled; \
        _debug_if = false, enable_gc())
#endif

#define SANITY_CHECK D_SANITY{ sanity_check_all(); }

#define debug(...) do{ int _debug_res = fprintf(stderr, __VA_ARGS__); guarantee_errno(_debug_res, "fprintf"); }while(0)

#ifndef ISABLE_DEBUG
extern bool debug_eval;
extern bool debug_hashtable;
extern bool debug_gc;
extern bool debug_atom;
extern bool debug_parse;
extern bool debug_sanity;
extern bool debug_string;
extern bool* debug_enabled;
extern bool always;
#endif

#endif

#include "term.h"
#include "render.h"
#include "roots.h"

#ifndef ISABLE_DEBUG
bool debug_eval = false;
bool debug_hashtable = false;
bool debug_gc = false;
bool debug_atom = false;
bool debug_parse = false;
bool debug_sanity = false;
bool debug_string = false;
extern bool base_loaded;
bool* debug_enabled = &base_loaded;
bool always = true;
#endif

void sanity_check_term(Term** term, ...){
    Term_render(*term, RENDER_STRICT | RENDER_NO_OP, MAX_PREC, MAX_PREC, false, do_nothing, NULL);
}

HEADER_DECLARE
void sanity_check_all(){
    static bool checking = false;
    if(checking) return;
    checking = true;
    each_root((term_iterator_t)sanity_check_term, NULL);
    checking = false;
}

