#include "genheader.h"

#include "roots.h"

#if HEADER_ONLY

#include "settings.h"
#include "hashtable.h"
#include "term_iterator.h"

struct roots_t {
    HashTable* globals;
    HashTable* ops;
    HashTable* interned;
    HashTable* atom_names;
    Term* nil;
    Term** c_terms[MAX_C_TERMS];
};

#endif

HEADER_DECLARE
struct roots_t root;

HEADER_DECLARE
size_t next_c_term = 0;

HEADER_DECLARE
void each_root(term_iterator_t f, void* data){
    HashTable_each_term(root.globals, f, data);
    HashTable_each_term(root.ops, f, data);
    HashTable_each_term(root.interned, f, data);
    HashTable_each_term(root.atom_names, f, data);
    if(root.nil){ f(&root.nil, data); }
    for(size_t i = 0; i < next_c_term; i++){
        if(*root.c_terms[i]){
            f(root.c_terms[i], data);
        }
    }
}
