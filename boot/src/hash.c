#include "genheader.h"

#include "hash.h"

#if HEADER_ONLY

#include <stdint.h>

#include "term.h"

typedef uint32_t hash_t;

#endif

#include "buffer.h"
#include "error.h"
#include "utils.h"

#define HASH_INIT 2166136261
#define HASH_PRIME 16777619

hash_t hash_byte(uint8_t c, hash_t hash){
    return (hash ^ c) * HASH_PRIME;
}

hash_t hash_string(Buffer* str, hash_t hash){
    for(size_t i = 0; i < str->end; i++){
        hash = hash_byte(str->ptr[i], hash);
    }
    return hash;
}

hash_t hash_integer(integer_t x, hash_t hash){
    char* c = (char*)&x;
    for(size_t i = 0; i < sizeof(x); i++){
        hash = hash_byte(c[i], hash);
    }
    return hash;
}

hash_t hash_atom(atom_t atom, hash_t hash){
    char* c = (char*)&atom;
    for(size_t i = 0; i < sizeof(atom); i++){
        hash = hash_byte(c[i], hash);
    }
    return hash;
}

HEADER_DECLARE
hash_t hash(Term* term){
    uint32_t hash = HASH_INIT;
    return hash_rec(term, hash);
}

HEADER_DECLARE
hash_t hash_rec(Term* term, hash_t hash){
    term = chase(term);
    switch(term->type){
    case INTEGER:
        return hash_integer(term->data.integer, hash);
    case FUNCTOR:
        hash = hash_atom(term->data.functor.atom, hash);
        functor_size_t size = term->data.functor.size;
        if(size){
            hash = hash_byte(size, hash);
            for(functor_size_t i = 0; i < size; i++){
                hash = hash_rec(term->data.functor.args[i], hash);
            }
        }
        return hash;
    case STRING:
        return hash_string(&term->data.string, hash);
    case VAR:
        fatal_error("Cannot hash variable '%s'", term->data.var.name);
    case DICT:
        fatal_error("unimplemented: hash dict");
    case MOVED:
        fatal_error("Cannot hash a moved term");
    default:
        UNREACHABLE;
    }
    UNREACHABLE;
}
