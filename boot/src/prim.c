#include "genheader.h"

#include "prim.h"

#if HEADER_ONLY

#include <stdbool.h>

#include "term.h"

typedef bool (*prim_t)(Term**);

#endif

#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
int kill(pid_t, int);

#include "stream.h"
#include "render.h"
#include "frame.h"
#include "hashtable.h"
#include "eval.h"
#include "list.h"
#include "alloc.h"
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

bool process_create(char* path, char** args, int* in, int* out, int* err, int* pid_out){
    int pipes[6];
    int res;
    guarantee_errno(!pipe(&pipes[0]), "pipe");
    guarantee_errno(!pipe(&pipes[2]), "pipe");
    guarantee_errno(!pipe(&pipes[4]), "pipe");
    int pid = fork();
    guarantee_errno(pid >= 0, "fork");
    if(pid == 0){
        int new_err = dup(2); guarantee_errno(new_err >= 0, "dup");
        res = dup2(pipes[0], 0); guarantee_errno(res >=0, "dup2");
        res = close(pipes[1]); guarantee_errno(res >=0, "close");
        res = close(pipes[2]); guarantee_errno(res >=0, "close");
        res = dup2(pipes[3], 1); guarantee_errno(res >=0, "dup2");
        res = close(pipes[4]); guarantee_errno(res >=0, "close");
        res = dup2(pipes[5], 2); guarantee_errno(res >=0, "dup2");
        Streams_close_after_fork();
        execvp(path, args);
        dup2(new_err, 2);
        guarantee_errno(false, "execvp");
        UNREACHABLE;
    }else{
        res = close(pipes[0]); guarantee_errno(res >=0, "close");
        *in = Stream_new(pipes[1]);
        *out = Stream_new(pipes[2]);
        res = close(pipes[3]); guarantee_errno(res >=0, "close");
        *err = Stream_new(pipes[4]);
        res = close(pipes[5]); guarantee_errno(res >=0, "close");
        *pid_out = pid;
        return true;
    }
}

void List_as_string_concat_into(Term* term, Buffer* buf){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(part) = Nil();
    for(; !Atom_eq(term, atom_nil); term = chase(List_tail(term))){
        part = chase(List_head(term));
        switch(part->type){
        case STRING:
            Buffer_append(buf, part->data.string.ptr, part->data.string.end);
            break;
        case FUNCTOR:
            guarantee(part->data.functor.size == 0, "non-empty functor in string");
            Buffer* b = atom_to_string(part->data.functor.atom);
            Buffer_append(buf, b->ptr, b->end);
            break;
        case INTEGER: {
            char c = part->data.integer;
            Buffer_append(buf, &c, 1);
            break;
        }
        case MOVED:
        case VAR:
        case DICT:
        default:
            fatal_error("invalid string");
            UNREACHABLE;
        }
    }
    FRAME_LEAVE;
}

Term* Term_String(Term* term){
    FRAME_ENTER_1(term);
    term = chase(term);
    if(term->type == STRING){
        FRAME_RETURN(Term*, term);
    }else if(Atom_eq(term, atom_nil)){
        FRAME_RETURN(Term*, String("",0));
    }else if(is_Atom(term)){
        FRAME_RETURN(Term*, atom_to_String(term->data.functor.atom));
    }else{
        FRAME_LOCAL(str) = String_unsafe(0);
        List_as_string_concat_into(term, &str->data.string);
        FRAME_RETURN(Term*, str);
    }
}

Buffer* Term_string(Term* term){
    return &Term_String(chase(term))->data.string;
}

Term* String_concat(Term* a, Term* b){
    FRAME_ENTER_2(a, b);
    D_STRING{
        trace_term("concat a", a);
        trace_term("concat b", b);
    }
    a = chase(a);
    b = chase(b);
    if(Atom_eq(a, atom_nil)){
        D_STRING{ debug("concat ret b"); }
        FRAME_RETURN(Term*, b);
    }
    if(a->type == STRING || is_Atom(a)){
        D_STRING{ debug("concat ret cons(a,b)\n"); }
        FRAME_RETURN(Term*, Functor2(atom_cons, a, b));
    }
    FRAME_LOCAL(tail) = Var(atom_underscore);
    FRAME_LOCAL(out) = tail;
    while(true){
        Term** args = Functor_get(a, atom_cons, 2);
        Term* head = chase(args[0]);
        if(args){
            if(head->type == INTEGER ||
               head->type == STRING ||
               is_Atom(head)){
                Var_push(&tail, head);
            }else{
                fatal_error("invalid string");
            }
            a = chase(args[1]);
        }else if(Atom_eq(a, atom_nil)){
            break;
        }else if(a->type == STRING || is_Atom(a)){
            Var_push(&tail, a);
            break;
        }else{
            fatal_error("invalid string");
        }
    }
    bool ok = unify(b, tail);
    guarantee(ok, "internal error: failed to unify string");
    D_STRING{ trace_term("concat ret", out); }
    FRAME_RETURN(Term*, out);
}

bool String_next_char(Term** str, size_t* n, char* out){
    if((*str)->type == VAR){
        return false;
    }
    if(Atom_eq(*str, atom_nil)){
        *str = NULL;
        return false;
    }
    Buffer* buf = NULL;
    if((*str)->type == STRING){
        buf = &(*str)->data.string;
    }else if(is_Atom(*str)){
        buf = atom_to_string((*str)->data.functor.atom);
    }
    if(buf){
        if(*n < buf->end){
            *out = buf->ptr[*n];
            (*n)++;
            return true;
        }else{
            *str = NULL;
            return false;
        }
    }
    Term** args = Functor_get(*str, atom_cons, 2);
    if(!args){
        fatal_error("invalid string");
    }
    Term* head = chase(args[0]);
    if(Atom_eq(head, atom_nil)){
        if(*n == 0){
            *out = '[';
            *n = 1;
            return true;
        }else{
            *out = ']';
            *n = 0;
            *str = args[1];
            return true;
        }
    }
    if(head->type == STRING || is_Atom(head)){
        Term* tmp = head;
        bool res = String_next_char(&tmp, n, out);
        if(res){
            return true;
        }else{
            *n = 0;
            *str = chase(args[1]);
            return String_next_char(str, n, out);
        }
    }
    if(head->type == INTEGER){
        *str = chase(args[1]);
        *out = head->data.integer;
        return true;
    }
    fatal_error("invalid string");
    UNREACHABLE;
}

Term* String_after(Term* str, size_t n){
    FRAME_ENTER_1(str);
    if(n == 0){
        FRAME_RETURN(Term*, str);
    }
    if(str->type == STRING || is_Atom(str)){
        Buffer* buf = Term_string(str);
        guarantee(buf->end >= n, "internal error: string too short");
        FRAME_RETURN(Term*, String(&buf->ptr[n], buf->end - n));
    }
    Term** args = Functor_get(str, atom_cons, 2);
    Term* head = chase(args[0]);
    if(head->type == STRING || is_Atom(head)){
        FRAME_RETURN(Term*, Functor2(atom_cons, String_after(head, n), args[1]));
    }
    fatal_error("internal error: invalid string");
    UNREACHABLE;
}

bool unify_strings(Term* a, Term* b){
    FRAME_ENTER_2(a, b);
    D_STRING{
        trace_term("unify a", a);
        trace_term("unify b", b);
    }
    size_t an = 0;
    size_t bn = 0;
    a = chase(a);
    b = chase(b);
    while(true){
        char ac;
        char bc;
        Term* prev_a = a;
        size_t prev_an = an;
        bool aeos = !String_next_char(&a, &an, &ac);
        if(aeos && a && a->type == VAR){
            FRAME_LOCAL(rest) = String_after(b, bn);
            D_STRING{ trace_term("partial b", rest); }
            FRAME_RETURN(bool, unify(a, rest));
        }
        bool beos = !String_next_char(&b, &bn, &bc);
        if(beos && b && b->type == VAR){
            FRAME_LOCAL(rest) = String_after(prev_a, prev_an);
            D_STRING{ trace_term("partial a", rest); }
            FRAME_RETURN(bool, unify(b, rest));
        }
        D_STRING{
            debug("next: eos (%d %d), c (%d %d), n (%zu %zu)\n", aeos, beos, ac, bc, an, bn);
            trace_term("next a", a);
            trace_term("next b", b);
        }
        if(aeos != beos || ac != bc){
            D_STRING{ debug("unify failed\n"); }
            FRAME_RETURN(bool, false);
        }
        if(aeos || beos){
            D_STRING{ debug("unify same\n"); }
            FRAME_RETURN(bool, true);
        }
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


bool prim_process_create(Term** args){
    disable_gc();
    char* command_path = Term_string(args[0])->ptr;
    char* command_args[256];
    command_args[0] = command_path;
    size_t n = 1;
    for(Term* list = args[1]; !Atom_eq(list, atom_nil); list = List_tail(list)){
        guarantee(n < sizeof(command_args) - 1, "too many arguments for process");
        command_args[n] = Term_string(List_head(list))->ptr;
        n++;
    }
    command_args[n] = NULL;
    int input_stream, output_stream, error_stream, pid;
    bool ret =
        process_create(command_path, command_args, &input_stream, &output_stream, &error_stream, &pid) &&
        unify(args[2], Integer(input_stream)) &&
        unify(args[3], Integer(output_stream)) &&
        unify(args[4], Integer(error_stream)) &&
        unify(args[5], Integer(pid));
    enable_gc();
    return ret;
}

bool prim_kill_process(Term** args){
    Term* pid = chase(args[0]);
    if(pid->type != INTEGER){
        fatal_error("kill_process expects an integer");
    }
    int res = kill(pid->data.integer, SIGTERM);
    guarantee_errno(res >= 0, "kill");
    return true;
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
    Buffer* str = Term_string(args[1]);
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

bool prim_string_codes(Term** args){
    FRAME_ENTER;
    FRAME_LOCAL(string) = chase(args[0]);
    FRAME_LOCAL(codes) = chase(args[1]);

    if(string->type == VAR){
        size_t size = List_length(codes);
        FRAME_LOCAL(term) = String_unsafe(size);
        size_t n = 0;
        FRAME_LOCAL(list) = codes;
        for(; !Atom_eq(list, atom_nil); list = List_tail(list)){
            term->data.string.ptr[n++] = Integer_get(List_head(list));
        }
        FRAME_RETURN(bool, unify(string, term));
    }else{
        FRAME_LOCAL(s) = Term_String(string);
        FRAME_LOCAL(list) = Var(atom_underscore);
        FRAME_LOCAL(tail) = list;
        for(size_t i = 0; i < s->data.string.end; i++){
            Var_push(&tail, Integer(s->data.string.ptr[i]));
        }
        set_var(tail, Nil());
        FRAME_RETURN(bool, unify(codes, list));
    }
}

bool prim_atom_string(Term** args){
    FRAME_ENTER;
    FRAME_LOCAL(atom) = chase(args[0]);
    FRAME_LOCAL(string) = chase(args[1]);
    if(is_Atom(atom)){
        FRAME_LOCAL(s) = atom_to_String(atom->data.functor.atom);
        FRAME_RETURN(bool, unify(string, s));
    }else{
        FRAME_RETURN(bool, unify(atom, Atom(intern(string))));
    }
}

bool prim_cons(Term** args){
    FRAME_ENTER;
    guarantee(Atom_eq(args[1], atom_nil), "load should be a singleton");
    FRAME_LOCAL(lib) = Functor_get(args[0], atom_library, 1)[0];
    Buffer* path = Buffer_empty(128);
    if(lib){
        Buffer* name = Term_string(lib);
        Buffer_append_nt(path, LIB_PATH);
        Buffer_append_nt(path, "/");
        Buffer_append_nt(path, name->ptr);
        Buffer_append_nt(path, ".pl");
    }else{
        Buffer* str = Term_string(List_head(args[0]));
        Buffer_append_nt(path, str->ptr);
    }
    load_file(path->ptr);
    Buffer_free(path);
    FRAME_RETURN(bool, true);
}

bool prim_string_concat(Term** args){
    FRAME_ENTER;
    Term* ta = chase(args[0]);
    Term* tb = chase(args[1]);
    Term* tc = chase(args[2]);
    if(ta->type == VAR){
        FRAME_LOCAL(sb) = Term_String(tb);
        FRAME_LOCAL(sc) = Term_String(tc);
        Buffer* b = &sb->data.string;
        Buffer* c = &sc->data.string;
        if(b->end > c->end || memcmp(b->ptr, &c->ptr[c->end - b->end], b->end)){
            return false;
        }
        FRAME_RETURN(bool, unify_strings(ta, String(c->ptr, c->end - b->end)));
    }else{
        FRAME_LOCAL(tab) = String_concat(ta, tb);
        D_STRING{ trace_term("concat res", tab); }
        FRAME_RETURN(bool, unify_strings(tc, tab));
    }
}

bool prim_string_first(Term** args){
    FRAME_ENTER;
    Term* str = chase(args[0]);
    Term* chr = chase(args[1]);
    if(chr->type == VAR){
        size_t n = 0;
        char c;
        bool res = String_next_char(&str, &n, &c);
        FRAME_RETURN(bool, res && unify_strings(chr, String(&c, 1)));
    }else{
        Term* s = Term_String(chr);
        guarantee(s->data.string.end == 1, "string_first: first argument is not a singleton");
        FRAME_LOCAL(var) = Var(atom_underscore);
        Term* concat_args[3];
        concat_args[0] = chr;
        concat_args[1] = var;
        concat_args[2] = str;
        FRAME_RETURN(bool, prim_string_concat(concat_args));
    }
}

bool prim_string(Term** args){
    return chase(args[0])->type == STRING;
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
