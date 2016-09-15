#include "genheader.h"

#include "atom.h"

#if HEADER_ONLY

#include <stdint.h>

#include "buffer.h"

typedef struct Term Term;

typedef uint64_t atom_t;

#define EACH_BUILTIN_ATOM(F) \
    F(atom_slash, "/") \
    F(atom_colon, ":") \
    F(atom_nil, "[]") \
    F(atom_cons, ".") \
    F(atom_op, "op") \
    F(atom_entails, ":-") \
    F(atom_frame, "frame") \
    F(atom_drop, "drop") \
    F(atom_comma, ",") \
    F(atom_eq, "=") \
    F(atom_empty, "empty") \
    F(atom_true, "true") \
    F(atom_underscore, "_") \
    F(atom_assertz_dcg, "assertz_dcg") \
    F(atom_long_rarrow, "-->") \
    F(atom_braces, "{}") \
    F(atom_library, "library") \
    F(atom_is, "is") \
    F(atom_add, "+") \
    F(atom_mul, "*") \
    F(atom_eof, "eof") \
    F(atom_xf, "xf") \
    F(atom_yf, "yf") \
    F(atom_xfx, "xfx") \
    F(atom_xfy, "xfy") \
    F(atom_yfx, "yfx") \
    F(atom_fx, "fx") \
    F(atom_fy, "fy") \
    F(atom_c_land, "c_land") \
    F(atom_cut, "!") \
    F(atom_false, "false") \
    F(atom_or, ";") \
    F(atom_cut_barrier, "cut_barrier") \
    F(atom_cut_passthrough, "cut_passthrough")

#define DECLARE_ATOM(a, _) a,
enum builtin_atoms_t {
    atom_invalid = 0,
    EACH_BUILTIN_ATOM(DECLARE_ATOM)
    first_free_atom
};

#endif

#include <stdio.h>
#include <string.h>

#include "term.h"
#include "hashtable.h"
#include "roots.h"
#include "error.h"
#include "frame.h"
#include "eval.h"

static atom_t next_free_atom = first_free_atom;

HEADER_DECLARE
Term* atom_to_String(atom_t atom){
    Term atom_term;
    atom_term.type = FUNCTOR;
    atom_term.data.functor.atom = atom;
    atom_term.data.functor.size = 0;
    atom_term.data.functor.args = NULL;
    Term* term = HashTable_find(root.atom_names, &atom_term);
    if(!term || Var_is_terminal(term)){
        char buf[20];
        snprintf(buf, 20, "?atom_%lu?", atom);
        return String_nt(buf);
    }
    guarantee(term->type == STRING, "internal error: atom names table contains non-string");
    return term;
}

HEADER_DECLARE
Buffer* atom_to_string(atom_t atom){
    return &atom_to_String(atom)->data.string;
}

HEADER_DECLARE
atom_t intern(Term* str){
    FRAME_ENTER_1(str);
    assert(str->type == STRING, "cannot intern non-string");
    FRAME_LOCAL(term) = chase(HashTable_get(root.interned, str));
    if(!Var_is_terminal(term)){
        guarantee(is_Atom(term), "interned term is not an atom");
        D_ATOM{
            debug("already interned `%s' as %lu\n", str->data.string.ptr, term->data.functor.atom);
        }
        FRAME_RETURN(atom_t, term->data.functor.atom);
    }
    atom_t atom = next_free_atom++;
    FRAME_LOCAL(tatom) = Atom(atom);
    set_var(term, tatom);
    FRAME_LOCAL(rev) = HashTable_get(root.atom_names, tatom);
    set_var(rev, str);
    D_ATOM{
        debug("interning %s as %lu\n", str->data.string.ptr, atom);
    }
    FRAME_RETURN(atom_t, atom);
}

HEADER_DECLARE
atom_t intern_nt(char* string){
    FRAME_ENTER;
    FRAME_LOCAL(s) = String_nt(string);
    FRAME_RETURN(atom_t, intern(s));
}

void intern_prim(char* string, atom_t atom){
    FRAME_ENTER;
    FRAME_LOCAL(str) = String_nt(string);
    FRAME_LOCAL(term) = HashTable_get(root.interned, str);
    if(!Var_is_terminal(term)){
        fatal_error("prim '%s' already exists", string);
    }
    set_var(term, Atom(atom));
    FRAME_LOCAL(rev) = HashTable_get(root.atom_names, term);
    set_var(rev, str);
    D_ATOM{
        debug("interning primitve %s as %lu\n", string, atom);
    }
    FRAME_LEAVE;
}

HEADER_DECLARE
void define_builtin_atoms(){
#define DEFINE_ATOM(name, string) intern_prim(string, name);
    EACH_BUILTIN_ATOM(DEFINE_ATOM)
}
