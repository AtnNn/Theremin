#include "genheader.h"

#include "vars.h"

#if HEADER_ONLY

#include "term.h"
#include "hashtable.h"

#endif

#include "frame.h"
#include "error.h"
#include "eval.h"
#include "list.h"

void list_vars(Term* term, HashTable* vars){
    FRAME_ENTER_1(term);
    term = chase(term);
    switch(term->type){
    case VAR:
        if(atom_to_string(term->data.var.name)->ptr[0] == '_') return;
        FRAME_LOCAL(val) = HashTable_get(vars, Integer((integer_t)term));
        if(Var_is_terminal(val)){
            set_var(val, term);
        }
        break;
    case FUNCTOR:
        for(functor_size_t i = 0; i < term->data.functor.size; i++){
            list_vars(term->data.functor.args[i], vars);
        }
        break;
    case DICT:
        fatal_error("unimplemented: list_vars dict");
    case MOVED:
    case INTEGER:
    case STRING:
    default:
        (void)0;
    }
    FRAME_LEAVE;
}

HEADER_DECLARE
Term* vars_of(Term* term){
    disable_gc();
    FRAME_ENTER_1(term);
    HashTable* vars = HashTable_new(PARSE_VARS_HASHTABLE_SIZE);
    list_vars(term, vars);
    FRAME_LOCAL(list) = Nil();
    FRAME_LOCAL(assoc) = NULL;
    FRAME_LOCAL(var) = NULL;
    for(size_t i = 0; i < vars->size; i++){
        assoc = vars->table[i];
        if(assoc){
            for(; !Atom_eq(assoc, atom_nil); assoc = List_tail(assoc)){
                Term** args = Functor_get(List_head(assoc), atom_colon, 2);
                var = Var(args[1]->data.var.ref->data.var.name);
                list = Functor2(atom_cons, Functor2(atom_eq, var, args[1]), list);
            }
        }
    }
    HashTable_free(vars);
    enable_gc();
    FRAME_RETURN(Term*, list);
}
