#include "genheader.h"

#include "term.h"
#include "frame.h"
#include "error.h"
#include "list.h"
#include "gc.h"
#include "render.h"

HEADER_DECLARE
Term* Assoc_get(Term** assoc, Term* key){
    FRAME_ENTER_1(key);
    for(Term* list = *assoc; !Atom_eq(list, atom_nil); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), atom_colon, 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            D_HASHTABLE{
                if(Atom_eq(args[1], atom_nil)){
                    trace_term("hash collision", key);
                }
            }
            FRAME_RETURN(Term*, args[1]);
        }
    }
    FRAME_LOCAL(var) = Var(atom_underscore);
    FRAME_LOCAL(pair) = Functor2(atom_colon, key, var);
    *assoc = Functor2(atom_cons, pair, *assoc);
    FRAME_RETURN(Term*, var);
}

HEADER_DECLARE
Term* Assoc_find(Term* assoc, Term* key){
    for(Term* list = assoc; !Atom_eq(list, atom_nil); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), atom_colon, 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            D_HASHTABLE{
                if(Atom_eq(args[1], atom_nil)){
                    trace_term("hash collision", key);
                }
            }
            return chase(args[1]);
        }
    }
    return NULL;
}
