#include "genheader.h"

#include "hashtable.h"

#if HEADER_ONLY

#include "term.h"
#include "term_iterator.h"

typedef struct HashTable {
    size_t size;
    Term* table[1];
} HashTable;

#endif

#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "assoc.h"
#include "hash.h"
#include "alloc.h"
#include "debug.h"
#include "gc.h"
#include "error.h"
#include "frame.h"
#include "render.h"
#include "list.h"
#include "eval.h"

HEADER_DECLARE
HashTable* HashTable_new(size_t size){
    HashTable* table = system_alloc(sizeof(HashTable) + sizeof(Term*) * (size - 1));
    D_HASHTABLE{
        debug("new hashtable %p of size %zu\n", (void*)table, size);
    }
    table->size = size;
    memset(table->table, 0, sizeof(Term*) * size);
    return table;
}

HEADER_DECLARE
void HashTable_free(HashTable* table){
    D_HASHTABLE{
        debug("freeing hashtable %p\n", (void*)table);
    }
    free(table);
}

HEADER_DECLARE
void HashTable_each_term(HashTable* table, term_iterator_t f, void* data){
    for(size_t i = 0; i < table->size; i++){
        if(table->table[i]){
            f(&table->table[i], data);
        }
    }
}

HEADER_DECLARE
Term* HashTable_get(HashTable* table, Term* key){
    FRAME_ENTER_1(key);
    hash_t hkey = hash(key);
    Term** assoc = &table->table[hkey % table->size];
    D_HASHTABLE{
        debug("hashtable %p get:\n", (void*)table);
        trace_term("key (hash %u)", key, hkey);
        trace_term("assoc", *assoc);
    }
    if(!*assoc){
        *assoc = Nil();
    }
    FRAME_LOCAL(val) = Assoc_get(assoc, key);
    D_HASHTABLE{
        trace_term("val", val);
    }
    FRAME_RETURN(Term*, val);
}

HEADER_DECLARE
void HashTable_append(HashTable* table, Term* key, Term* val){
    FRAME_ENTER_2(key, val);
    FRAME_LOCAL(list) = HashTable_get(table, key);
    FRAME_LOCAL(tail) = NULL;
    for(list = chase(list); list->type != VAR; list = chase(List_tail(list))){ }
    set_var(list, Functor2(atom_cons, val, Var(atom_underscore)));
    D_HASHTABLE{
        debug("hashtable %p: append\n", (void*)table);
        trace_term("key", key);
        trace_term("val", val);
    }
    FRAME_LEAVE;
}

HEADER_DECLARE
Term* HashTable_find(HashTable* table, Term* key){
    Term *assoc = table->table[hash(key) % table->size];
    if(!assoc){
        return NULL;
    }
    return Assoc_find(assoc, key);
}
