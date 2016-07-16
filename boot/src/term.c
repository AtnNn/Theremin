#include "genheader.h"

#include "term.h"

#if HEADER_ONLY

#include <stdbool.h>

#include "atom.h"
#include "buffer.h"

typedef uint8_t functor_size_t;

typedef int64_t integer_t;

struct Term {
    enum { FUNCTOR, VAR, MOVED, INTEGER, STRING, DICT } type;
    union {
        integer_t integer;
        Buffer string;
        struct Term* moved_to;
        struct {
            atom_t atom;
            functor_size_t size;
            struct Term** args;
        } functor;
        struct {
            atom_t name;
            struct Term* ref;
        } var;
        struct HashTable* dict;
    } data;
};

#endif

#include "utils.h"
#include "gc.h"
#include "error.h"
#include "alloc.h"
#include "frame.h"

HEADER_DECLARE
Term* chase(Term* term){
    while(term->type == VAR && term->data.var.ref != term){
        term = term->data.var.ref;
    }
    return term;
}

HEADER_DECLARE
Term* Integer(integer_t n){
    Term* term = Term_new_gc();
    term->type = INTEGER;
    term->data.integer = n;
    return term;
}

HEADER_DECLARE
Term* String(char* ptr, size_t size){
    Term* term = Term_new_gc();
    term->type = STRING;
    term->data.string.ptr = NULL;
    term->data.string.alloc_size = 0;
    term->data.string.end = 0;
    Buffer_append(&term->data.string, ptr, size);
    return term;
}

HEADER_DECLARE
Term* String_unsafe(size_t size){
    Term* term = Term_new_gc();
    term->type = STRING;
    term->data.string.ptr = NULL;
    term->data.string.alloc_size = 0;
    term->data.string.end = 0;
    Buffer_reserve(&term->data.string, size);
    term->data.string.end = size;
    return term;
}

HEADER_DECLARE
Term* String_nt(char* str){
    Term* term = Term_new_gc();
    term->type = STRING;
    term->data.string.ptr = NULL;
    term->data.string.alloc_size = 0;
    term->data.string.end = 0;
    Buffer_append_nt(&term->data.string, str);
    return term;
}

HEADER_DECLARE
Term* Atom(atom_t atom){
    if(!atom){
        fatal_error("internal error: atom is null");
    }
    Term* term = Term_new_gc();
    term->type = FUNCTOR;
    term->data.functor.atom = atom;
    term->data.functor.size = 0;
    term->data.functor.args = NULL;
    return term;
}

HEADER_DECLARE
Term* Functor_unsafe(atom_t atom, functor_size_t size){
    Term* term = Term_new_gc();
    term->type = FUNCTOR;
    term->data.functor.atom = atom;
    term->data.functor.size = size;
    term->data.functor.args = system_alloc(sizeof(Term*) * size);
    for(functor_size_t i = 0; i < size; i++){
        term->data.functor.args[i] = NULL;
    }
    return term;
}

HEADER_DECLARE
void Functor_set_arg(Term* term, functor_size_t n, Term* arg){
    guarantee(term->type == FUNCTOR, "Functor_set_arg: not a functor");
    guarantee(term->data.functor.size > n, "Functor_set_arg: too few arguments");
    term->data.functor.args[n] = arg;
}

HEADER_DECLARE
Term* Functor1(atom_t atom, Term* a){
    FRAME_ENTER_1(a);
    Term* term = Functor_unsafe(atom, 1);
    Functor_set_arg(term, 0, a);
    FRAME_RETURN(Term*, term);
}

HEADER_DECLARE
Term* Functor2(atom_t atom, Term* a, Term* b){
    FRAME_ENTER_2(a, b);
    Term* term = Functor_unsafe(atom, 2);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    FRAME_RETURN(Term*, term);
}

HEADER_DECLARE
Term* Functor3(atom_t atom, Term* a, Term* b, Term* c){
    FRAME_ENTER_3(a, b, c);
    Term* term = Functor_unsafe(atom, 3);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    FRAME_RETURN(Term*, term);
}

HEADER_DECLARE
Term* Functor4(atom_t atom, Term* a, Term* b, Term* c, Term* d){
    FRAME_ENTER_4(a, b, c, d);
    Term* term = Functor_unsafe(atom, 4);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    FRAME_RETURN(Term*, term);
}

HEADER_DECLARE
Term* Functor5(atom_t atom, Term* a, Term* b, Term* c, Term* d, Term* e){
    FRAME_ENTER_5(a, b, c, d, e);
    Term* term = Functor_unsafe(atom, 5);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    Functor_set_arg(term, 4, e);
    FRAME_RETURN(Term*, term);
}

HEADER_DECLARE
Term* Var(atom_t name){
    Term* term = Term_new_gc();
    term->type = VAR;
    term->data.var.name = name;
    term->data.var.ref = term;
    return term;
}

HEADER_DECLARE
bool Var_is_terminal(Term* term){
    return term->type == VAR && term->data.var.ref == term;
}

HEADER_DECLARE
void Term_destroy(Term* term){
    switch(term->type){
    case MOVED:
        return;
    case VAR:
        break;
    case FUNCTOR:
        free(term->data.functor.args);
        break;
    case INTEGER:
        break;
    case STRING:
        free(term->data.string.ptr);
        break;
    case DICT:
        HashTable_free(term->data.dict);
        break;
    }
}

HEADER_DECLARE
Term** Functor_get(Term* term, atom_t atom, functor_size_t size){
    if(term->type != FUNCTOR ||
       term->data.functor.atom != atom ||
       term->data.functor.size != size){
        return NULL;
    }
    return term->data.functor.args;
}

HEADER_DECLARE
bool is_Atom(Term* term){
    return term->type == FUNCTOR && term->data.functor.size == 0;
}

HEADER_DECLARE
bool Atom_eq(Term* term, atom_t atom){
    if(term->type != FUNCTOR ||
       term->data.functor.atom != atom ||
       term->data.functor.size != 0){
        return false;
    }
    return true;
}

HEADER_DECLARE
bool Term_exact_eq(Term* a, Term* b){
    a = chase(a);
    b = chase(b);
    if(a->type != b->type){
        return false;
    }
    switch(a->type){
    case MOVED:
        fatal_error("Found a moved term when comparing terms");
    case VAR:
        return a == b;
    case INTEGER:
        return a->data.integer == b->data.integer;
    case STRING:
        return !Buffer_cmp(&a->data.string, &b->data.string);
    case FUNCTOR:
        if(a->data.functor.atom != b->data.functor.atom){
            return false;
        }
        if(a->data.functor.size != b->data.functor.size){
            return false;
        }
        for(int i = 0; i < a->data.functor.size; i++){
            if(!Term_exact_eq(a->data.functor.args[i], b->data.functor.args[i])){
                return false;
            }
        }
        return true;
    case DICT:
        fatal_error("unimplemented: exact_eq dict");
    }
    UNREACHABLE;
}

HEADER_DECLARE
integer_t Integer_get(Term* term){
    term = chase(term);
    if(term->type != INTEGER){
        fatal_error("expected integer");
    }
    return term->data.integer;
}

HEADER_DECLARE
Term* Spec(atom_t atom, int size){
    FRAME_ENTER;
    FRAME_LOCAL(tatom) = Atom(atom);
    FRAME_LOCAL(tsize) = Integer(size);
    FRAME_RETURN(Term*, Functor2(atom_slash, tatom, tsize));
}

HEADER_DECLARE
Term* Dict(size_t size){
    Term* term = Term_new_gc();
    term->type = DICT;
    term->data.dict = HashTable_new(size);
    return term;
}
