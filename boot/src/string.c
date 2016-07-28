// TODO: better string representation to avoid allocation in string_first

#include "genheader.h"

#include "string.h"

#if HEADER_ONLY

#include "term.h"

#endif

#include "frame.h"
#include "error.h"
#include "list.h"
#include "utils.h"
#include "render.h"
#include "eval.h"

Term* String_pack_list(Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(part) = Nil();
    FRAME_LOCAL(ret) = String_unsafe(0);
    for(; !Atom_eq(term, atom_nil); term = chase(List_tail(term))){
        part = chase(List_head(term));
        switch(part->type){
        case STRING:
            Buffer_append(&ret->data.string, part->data.string.ptr, part->data.string.end);
            break;
        case FUNCTOR:
            guarantee(part->data.functor.size == 0, "non-empty functor in string");
            Buffer* b = atom_to_string(part->data.functor.atom);
            Buffer_append(&ret->data.string, b->ptr, b->end);
            break;
        case INTEGER: {
            char c = part->data.integer;
            Buffer_append(&ret->data.string, &c, 1);
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
    FRAME_RETURN(Term*, ret);
}

Term* String_pack(Term* term){
    FRAME_ENTER_1(term);
    term = chase(term);
    if(term->type == STRING){
        FRAME_RETURN(Term*, term);
    }else if(Atom_eq(term, atom_nil)){
        FRAME_RETURN(Term*, String("",0));
    }else if(is_Atom(term)){
        FRAME_RETURN(Term*, atom_to_String(term->data.functor.atom));
    }else{
        FRAME_RETURN(Term*, String_pack_list(term));
    }
}

HEADER_DECLARE
Buffer* String_pack_buf(Term* term){
    return &String_pack(chase(term))->data.string;
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
        if(args){
            Term* head = chase(args[0]);
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
        Buffer* buf = String_pack_buf(str);
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

HEADER_DECLARE
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
        FRAME_LOCAL(s) = String_pack(string);
        FRAME_LOCAL(list) = Var(atom_underscore);
        FRAME_LOCAL(tail) = list;
        for(size_t i = 0; i < s->data.string.end; i++){
            Var_push(&tail, Integer(s->data.string.ptr[i]));
        }
        set_var(tail, Nil());
        FRAME_RETURN(bool, unify(codes, list));
    }
}

HEADER_DECLARE
bool prim_atom_string(Term** args){
    FRAME_ENTER;
    FRAME_LOCAL(atom) = chase(args[0]);
    FRAME_LOCAL(string) = chase(args[1]);
    if(is_Atom(atom)){
        FRAME_LOCAL(s) = atom_to_String(atom->data.functor.atom);
        FRAME_RETURN(bool, unify_strings(string, s));
    }else{
        FRAME_RETURN(bool, unify(atom, Atom(intern(String_pack(string)))));
    }
}

HEADER_DECLARE
bool prim_string_concat(Term** args){
    FRAME_ENTER;
    Term* ta = chase(args[0]);
    Term* tb = chase(args[1]);
    Term* tc = chase(args[2]);
    if(ta->type == VAR){
        FRAME_LOCAL(sb) = String_pack(tb);
        FRAME_LOCAL(sc) = String_pack(tc);
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

HEADER_DECLARE
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
        Term* s = String_pack(chr);
        guarantee(s->data.string.end == 1, "string_first: first argument is not a singleton");
        FRAME_LOCAL(var) = Var(atom_underscore);
        Term* concat_args[3];
        concat_args[0] = chr;
        concat_args[1] = var;
        concat_args[2] = str;
        FRAME_RETURN(bool, prim_string_concat(concat_args));
    }
}

HEADER_DECLARE
bool prim_string(Term** args){
    return chase(args[0])->type == STRING;
}
