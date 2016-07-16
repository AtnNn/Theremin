#include "genheader.h"

#include "term_copy.h"

#if HEADER_ONLY

#include "term.h"

#endif

#include "hashtable.h"
#include "frame.h"
#include "error.h"
#include "utils.h"

Term* Term_copy_rec(Term* term, HashTable* vars){
    term = chase(term);
    switch(term->type){
    case INTEGER:
    case STRING:
        return term;
    case FUNCTOR:{
        functor_size_t size = term->data.functor.size;
        Term** args = term->data.functor.args;
        Term* copy = Functor_unsafe(term->data.functor.atom, size);
        for(int i = 0; i < size; i++){
            Functor_set_arg(copy, i, Term_copy_rec(args[i], vars));
        }
        return copy;
    }
    case VAR: {
        Term* copy = HashTable_get(vars, Integer((integer_t)term)); //TODO: don't cast pointer to integer
        assert(copy->type == VAR && (copy->data.var.name == atom_underscore || copy->data.var.name == term->data.var.name), "invalid variable in hashtable");
        copy->data.var.name = term->data.var.name;
        return copy;
    }
    case DICT:
        fatal_error("unimplemented: copyrec dict");
    case MOVED:
        fatal_error("Cannot copy a moved term");
        return NULL;
    default:
        UNREACHABLE;
    }
}

HEADER_DECLARE
Term* Term_copy(Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(vars) = Dict(COLLISION_HASHTABLE_SIZE);
    FRAME_RETURN(Term*, Term_copy_rec(term, vars->data.dict));
}
