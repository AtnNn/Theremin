#include "genheader.h"

#include "gc.h"

#if HEADER_ONLY

#include "term.h"

typedef struct Pool {
    size_t sections;
    size_t free;
    Term** terms;
} Pool;

#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "roots.h"
#include "alloc.h"
#include "debug.h"
#include "error.h"
#include "utils.h"

int gc_disable_count = 0;
bool would_gc = false;

HEADER_DECLARE
Pool* pool = NULL;

Pool* Pool_new(){
    Pool* ret = system_alloc(sizeof(Pool));
    ret->sections = 1;
    ret->free = 0;
    ret->terms = system_alloc(sizeof(Term*));
    ret->terms[0] = system_alloc(sizeof(Term) * POOL_SECTION_SIZE);
    return ret;
}

void trace_pool_info(char* str, Pool* p){
    debug("%s: %zu used, %zu available in %zu sections\n",
          str,  p->free, p->sections * POOL_SECTION_SIZE,
          p->sections);
}


void Pool_free(Pool* p){
    D_GC{ trace_pool_info("freeing", p); }
    size_t remaining = p->free;
    for(size_t i = 0; i < p->sections; i++){
        for(size_t j = 0; j < POOL_SECTION_SIZE; j++){
            Term_destroy(&p->terms[i][j]);
            remaining--;
            if(!remaining){
                goto outer;
            }
        }
        free(p->terms[i]);
    }
 outer:
    free(p->terms);
    free(p);
}

void Pool_expand(Pool* p){
    D_GC{ trace_pool_info("expanding", p); }
    p->sections++;
    p->terms = system_realloc(p->terms, sizeof(Term*) * p->sections);
    p->terms[p->sections-1] = system_alloc(sizeof(Term) * POOL_SECTION_SIZE);
}

Term* Pool_add_term_expand(Pool* p){
     if(p->free >= p->sections * POOL_SECTION_SIZE){
        Pool_expand(p);
     }
     Term* term = &p->terms[p->free / POOL_SECTION_SIZE][p->free % POOL_SECTION_SIZE];
     p->free++;
     return term;
}

void Pool_pour(Term** term, Pool *p){
    if((*term)->type == MOVED){
        *term = (*term)->data.moved_to;
    }else{
        Term* new = Pool_add_term_expand(p);
        memcpy(new, *term, sizeof(Term));
        (*term)->type = MOVED;
        (*term)->data.moved_to = new;
        *term = new;
        switch(new->type){
        case VAR:
            Pool_pour(&new->data.var.ref, p);
            break;
        case FUNCTOR:
            for(functor_size_t i = 0; i < new->data.functor.size; i++){
                Pool_pour(&new->data.functor.args[i], p);
            }
            break;
        case DICT:
            HashTable_each_term(new->data.dict, (term_iterator_t)Pool_pour, (void*)p);
            break;
        case INTEGER:
        case STRING:
            break;
        case MOVED:
            UNREACHABLE;
        }
    }
}

void gc(Pool** p){
    D_GC{ trace_pool_info("start gc", *p); }
    Pool *new = Pool_new();
    D_GC{ trace_pool_info("new pool", new); }
    each_root((term_iterator_t)Pool_pour, new);
    Pool_free(*p);
    Pool_expand(new);
    *p = new;
    D_GC{ trace_pool_info("finished gc", *p); }
}

HEADER_DECLARE
void disable_gc(){
    gc_disable_count++;
}

HEADER_DECLARE
void enable_gc(){
    gc_disable_count--;
}

HEADER_DECLARE
Term* Term_new_gc(){
    if(pool->free >= pool->sections * POOL_SECTION_SIZE){
        if(!gc_disable_count){
            gc(&pool);
        }
     }
     Term* term = Pool_add_term_expand(pool);
     return term;
}

HEADER_DECLARE
void gc_init(){
    pool = Pool_new();
}
