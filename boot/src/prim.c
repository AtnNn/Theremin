#include "genheader.h"

#include "prim.h"

#if HEADER_ONLY

#include <stdbool.h>

#include "term.h"

typedef bool (*prim_t)(Term**);

#endif

#include <unistd.h>
#include <string.h>

#include "stream.h"
#include "render.h"
#include "frame.h"
#include "hashtable.h"
#include "eval.h"
#include "list.h"
#include "alloc.h"
#include "string.h"
#include "process.h"

bool prim_print(Term** args){
    Term_print(args[0]);
    return true;
}

bool prim_fail(){
    return false;
}

bool prim_true(){
    return true;
}

bool prim_op(Term** args){
    FRAME_ENTER;
    FRAME_LOCAL(prec) = chase(args[0]);
    FRAME_LOCAL(type) = chase(args[1]);
    FRAME_LOCAL(name) = chase(args[2]);
    guarantee(
              prec->type == INTEGER &&
              type->type == FUNCTOR &&
              name->type == FUNCTOR,
              "invalid op spec");
    HashTable_append(root.ops, name, Functor3(atom_op, prec, type, name));
    FRAME_RETURN(bool, true);
}

bool prim_unify(Term** args){
    D_EVAL{
        trace_term("unify a", args[0]);
        trace_term("unify b", args[1]);
    }
    bool res = unify(args[0], args[1]);
    D_EVAL{ debug("unify res: %s\n", res ? "true" : "fail"); }
    return res;
}

bool prim_nl(){
    printf("\n");
    return true;
}

bool prim_cut(){
    Term** frame = Functor_get(current_eval_env->stack, atom_frame, 3);
    frame[0] = Nil();
    return true;
}

bool prim_assertz(Term** args){
    return assertz(args[0]);
}

integer_t eval_math(Term* expr){
    expr = chase(expr);
    D_EVAL{ trace_term("eval_math", expr); }
    switch(expr->type){
    case INTEGER:
        return expr->data.integer;
    case FUNCTOR: {
        atom_t atom = expr->data.functor.atom;
        functor_size_t size = expr->data.functor.size;
        Term** args = expr->data.functor.args;
        if(atom == atom_add && size == 2){
            return eval_math(args[0]) + eval_math(args[1]);
        }else if(atom == atom_mul && size == 2){
            return eval_math(args[0]) * eval_math(args[1]);
        }
    }
    case VAR:
    case MOVED:
    case STRING:
    case DICT:
    default:
        fatal_error("invalid math expression");
        UNREACHABLE;
    }
}

bool prim_is(Term** args) {
    return unify(args[0], Integer(eval_math(chase(args[1]))));
}

bool prim_univ(Term** args){
    FRAME_ENTER;
    FRAME_LOCAL(functor) = chase(args[0]);
    FRAME_LOCAL(repr) = chase(args[1]);
    if(functor->type == VAR){
        FRAME_LOCAL(name) = chase(List_head(repr));
        guarantee(is_Atom(name), "atom expected in '=..'/2");
        functor_size_t size = 0;
        for(Term* list = chase(List_tail(repr));
            !Atom_eq(list, atom_nil);
            list = chase(List_tail(list))){
            size++;
        }
        Term* term = Functor_unsafe(name->data.functor.atom, size);
        functor_size_t i = 0;
        for(Term* list = chase(List_tail(repr));
            !Atom_eq(list, atom_nil);
            list = chase(List_tail(list)), i++){
            Functor_set_arg(term, i, chase(List_head(list)));
        }
        FRAME_TRACK_VAR(term);
        FRAME_RETURN(bool, unify(functor, term));
    }else if(functor->type == FUNCTOR){
        FRAME_LOCAL(tail) = Var(atom_underscore);
        FRAME_LOCAL(list) = Functor2(atom_cons, Atom(functor->data.functor.atom), tail);
        for(functor_size_t i = 0; i < functor->data.functor.size; i++){
            Var_push(&tail, functor->data.functor.args[i]);
        }
        set_var(tail, Nil());
        FRAME_RETURN(bool, unify(repr, list));
    }else{
        FRAME_RETURN(bool, false);
        UNREACHABLE;
    }
}

bool prim_close(Term** args){
    Term* stream = chase(args[0]);
    if(stream->type != INTEGER){
        fatal_error("close expects an integer");
    }
    Stream_close(stream->data.integer);
    return true;
}

bool prim_write_string(Term** args){
    int stream = Integer_get(args[0]);
    Buffer* str = String_pack_buf(args[1]);
    ssize_t res = write(Stream_get(stream)->fd, str->ptr, str->end);
    if(res >= 0){
        return true;
    }else{
        debug("warning: write failed: %s\n", strerror(errno));
        return false;
    }
}

bool prim_read_string(Term** args){
    integer_t stream_id = Integer_get(args[0]);
    integer_t max = Integer_get(args[1]);
    char buf[max];
    Stream* stream = Stream_get(stream_id);
    guarantee(max > 0, "invalid argument to read_string");
    if(stream->size > (size_t)max){
        bool ret = unify(args[2], String(stream->buf + stream->pos, max));
        stream->pos += max;
        return ret;
    }else if(stream->size > 0){
        bool ret = unify(args[2], String(stream->buf + stream->pos, stream->size));
        stream->pos = 0;
        stream->size = 0;
        free(stream->buf);
        stream->buf = NULL;
        stream->alloc_size = 0;
        return ret;
    }else{
        ssize_t res = read(stream->fd, buf, max);
        if(res == 0){
            return unify(args[2], Atom(atom_eof));
        }else if(res >= 1){
            return unify(args[2], String(buf, res));
        }else{
            debug("warning: read failed: %s\n", strerror(errno));
            return false;
        }
    }
}

bool prim_cons(Term** args){
    FRAME_ENTER;
    guarantee(Atom_eq(args[1], atom_nil), "load should be a singleton");
    FRAME_LOCAL(lib) = Functor_get(args[0], atom_library, 1)[0];
    Buffer* path = Buffer_empty(128);
    if(lib){
        Buffer* name = String_pack_buf(lib);
        Buffer_append_nt(path, LIB_PATH);
        Buffer_append_nt(path, "/");
        Buffer_append_nt(path, name->ptr);
        Buffer_append_nt(path, ".pl");
    }else{
        Buffer* str = String_pack_buf(List_head(args[0]));
        Buffer_append_nt(path, str->ptr);
    }
    load_file(path->ptr);
    Buffer_free(path);
    FRAME_RETURN(bool, true);
}

bool prim_var(Term** args){
    return chase(args[0])->type == VAR;
}

bool prim_listing(Term** args){
    FRAME_ENTER;
    FRAME_LOCAL(rules) = HashTable_find(root.globals, chase(args[0]));
    if(!rules){
        FRAME_RETURN(bool, false);
    }
    for(; !Var_is_terminal(rules); rules = chase(List_tail(rules))){
        Term_print(List_head(rules));
        printf("\n");
    }
    FRAME_RETURN(bool, true);
}

HEADER_DECLARE
prim_t find_prim(atom_t atom, functor_size_t size){

#define PRIM(f, n, r) \
    static atom_t atom_ ## r = 0; \
    if(!atom_ ## r) atom_ ## r = intern_nt(#f); \
    if(atom == atom_ ## r && size == n){ return prim_ ## r; }

    PRIM(print, 1, print);
    PRIM(fail, 0, fail);
    PRIM(true, 0, true);
    PRIM(=, 2, unify);
    PRIM(nl, 0, nl);
    PRIM(op, 3, op);
    PRIM(!, 0, cut);
    PRIM(assertz, 1, assertz);
    PRIM(=.., 2, univ);
    PRIM(is, 2, is);
    PRIM(process_create, 6, process_create);
    PRIM(kill_process, 1, kill_process);
    PRIM(close, 1, close);
    PRIM(write_string, 2, write_string);
    PRIM(read_string, 3, read_string);
    PRIM(string_codes, 2, string_codes);
    PRIM(atom_string, 2, atom_string);
    PRIM(., 2, cons);
    PRIM(string_concat, 3, string_concat);
    PRIM(string, 1, string);
    PRIM(string_first, 2, string_first);
    PRIM(var, 1, var);
    PRIM(listing,1,listing);
#undef PRIM

    return NULL;
}
