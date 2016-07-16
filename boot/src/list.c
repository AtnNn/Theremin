#include "genheader.h"

#include "list.h"

#if HEADER_ONLY

#include "term.h"

#endif

#include "roots.h"
#include "render.h"
#include "error.h"
#include "frame.h"
#include "eval.h"

HEADER_DECLARE
Term* Nil(){
    if(!root.nil){
        root.nil = Atom(atom_nil);
    }
    return root.nil;
}

HEADER_DECLARE
Term* List_head(Term* list){
    Term** args = Functor_get(list, atom_cons, 2);
    if(!args){
        trace_term("list", list);
        fatal_error("expected non-empty list");
    }
    return args[0];
}

HEADER_DECLARE
Term* List_tail(Term* list){
    Term** args = Functor_get(list, atom_cons, 2);
    if(!args){
        trace_term("list", list);
        fatal_error("expected non-empty list");
    }
    return args[1];
}

HEADER_DECLARE
integer_t List_length(Term* list){
    integer_t ret = 0;
    for(; !Atom_eq(list, atom_nil); list = List_tail(list)){
        ret++;
    }
    return ret;
}

HEADER_DECLARE
void Var_push(Term** var, Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(ret) = Var(atom_underscore);
    bool ok = unify(*var, Functor2(atom_cons, term, ret));
    guarantee(ok, "internal error: failed to unify");
    *var = ret;
    FRAME_LEAVE;
}
