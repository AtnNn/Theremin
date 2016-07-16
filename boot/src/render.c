#include "genheader.h"

#include "render.h"

#if HEADER_ONLY

#include "term.h"
#include "grammar.h"

enum render_flags_t {
    RENDER_DEFAULT = 0,
    RENDER_NO_CHASE = 1,
    RENDER_STRICT = 2,
    RENDER_NO_OP = 4
};

typedef void (*renderer_t)(void*, char*, size_t);

#endif

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdarg.h>

#include "frame.h"
#include "error.h"
#include "assert.h"
#include "list.h"

bool render_op(Term* term, int render_flags, int left_prec, int right_prec, bool in_list, renderer_t write, void* data){
    FRAME_ENTER_1(term);
    assert(term->type == FUNCTOR, "not a functor");
    if(term->data.functor.size > 2 || term->data.functor.size == 0){
        FRAME_RETURN(bool, false);
    }
    Term** cons = Functor_get(term, atom_cons, 2);
    if(cons){
        FRAME_LOCAL(car) = cons[0];
        FRAME_LOCAL(cdr) = chase(cons[1]);
        write(data, "[", 1);
        while(true){
            Term_render(car, render_flags, MAX_PREC, MAX_PREC, true, write, data);
            Term** cons = Functor_get(cdr, atom_cons, 2);
            if(cons){
                car = cons[0];
                cdr = chase(cons[1]);
                write(data, ", ", 2);
                continue;
            }else if(Atom_eq(cdr, atom_nil)){
                break;
            }else{
                write(data, " | ", 3);
                Term_render(cdr, render_flags, MAX_PREC, MAX_PREC, true, write, data);
                break;
            }
        }
        write(data, "]", 1);
        FRAME_RETURN(bool, true);
    }
    FRAME_LOCAL(list) = HashTable_find(root.ops, Atom(term->data.functor.atom));
    if(!list){
        FRAME_RETURN(bool, false);
    }
    int inner_prec;
    atom_t type;
    while(true){
        if(Atom_eq(list, atom_nil)){
            FRAME_RETURN(bool, false);
        }
        Term** args = Functor_get(List_head(list), atom_op, 3);
        inner_prec = Integer_get(args[0]);
        assert(args[1]->type == FUNCTOR, "invalid op spec");
        type = args[1]->data.functor.atom;
        if(op_type_arg_size(type) == term->data.functor.size){
            break;
        }
        list = List_tail(list);
    }
    integer_t left;
    integer_t right;
    op_type(inner_prec, type, &left, &right);
    bool parens = left > left_prec || right > right_prec;

    if(in_list && term->data.functor.atom == atom_comma){
        parens = true;
    }

    if(parens){
        left_prec = MAX_PREC;
        right_prec = MAX_PREC;
        write(data, "(", 1);
    }
    int next = 0;
    if(left){
        FRAME_LOCAL(arg) = term->data.functor.args[next++];
        Term_render(arg, render_flags, left_prec, left, false, write, data);
    }
    Buffer* s = atom_to_string(term->data.functor.atom);
    if(!strchr(",", s->ptr[0])){
        write(data, " ", 1);
    }
    write(data, s->ptr, s->end);
    write(data, " ", 1);
    if(right){
        FRAME_LOCAL(arg) = term->data.functor.args[next];
        Term_render(arg, render_flags, right, right_prec, false, write, data);
    }
    if(parens){
        write(data, ")", 1);
    }

    FRAME_RETURN(bool, true);
}

HEADER_DECLARE
void Term_render(Term* term, int render_flags, int left_prec, int right_prec, bool in_list, renderer_t write, void* data){
    if(!term){
        if(render_flags & RENDER_STRICT){
            fatal_error("null term");
        }
        write(data, "?null?", 5);
        return;
    }
    if(!(render_flags & RENDER_NO_CHASE)){
        term = chase(term);
    }
    switch(term->type){
    case MOVED:
        if(render_flags & RENDER_STRICT){
            fatal_error("encountered garbage collected term");
        }
        write(data, "?moved?", 7);
        break;
    case VAR:
        if(term->data.var.ref != term){
            Term_render(term->data.var.ref, render_flags, left_prec, right_prec, in_list, write, data);
        }else{
            Buffer* name = atom_to_string(term->data.var.name);
            write(data, name->ptr, name->end);
        }
        break;
    case INTEGER: {
        char buf[16];
        int n = sprintf(buf, "%ld", term->data.integer);
        write(data, buf, n);
        break;
    }
    case STRING:
        write(data, "\"", 1);
        write(data, term->data.string.ptr, term->data.string.end);
        write(data, "\"", 1);
        break;
    case FUNCTOR:
        if (render_flags & RENDER_NO_OP || !render_op(term, render_flags, left_prec, right_prec, in_list, write, data)) {
            Buffer* name = atom_to_string(term->data.functor.atom);
            write(data, name->ptr, name->end);
            if(term->data.functor.size){
                write(data, "(", 1);
                for(int i = 0; i < term->data.functor.size; i++){
                    Term_render(term->data.functor.args[i], render_flags, MAX_PREC, MAX_PREC, true, write, data);
                    if(i + 1 < term->data.functor.size){
                        write(data, ", ", 2);
                    }
                }
                write(data, ")", 1);
            }
        }
        break;
    case DICT:
        write(data, "?dict?", 6);
    default:
        if(render_flags & RENDER_STRICT){
            fatal_error("invalid term type");
        }
        write(data, "?invalid?", 9);
    }
}

HEADER_DECLARE
Buffer* Term_show(Term* term, int render_flags){
    Buffer* buffer = Buffer_empty(0);
    Term_render(term, render_flags, MAX_PREC, MAX_PREC, false, (renderer_t)Buffer_append, buffer);
    Buffer_shrink(buffer);
    return buffer;
}

HEADER_DECLARE
char* short_snippet(char* str, char* buf, size_t size){
    size--;
    size_t j = 0;
    bool space = true;
    for(size_t i = 0; j < size && str[i]; i++){
        char c = str[i];
        if(isspace(c)){
            if(space){
                continue;
            }else{
                c = ' ';
                space = true;
            }
        }else{
            space = false;
        }
        buf[j++] = c;
    }
    if(j == size){
        strcpy(buf + size - 3, "...");
    }
    buf[j] = 0;
    return buf;
}

HEADER_DECLARE
void trace_term(char* format, Term* term, ...){
    char buf[80];
    va_list argptr;
    va_start(argptr, term);
    vfprintf(stderr, format, argptr);
    Buffer* buffer = Term_show(term, 0);
    debug(": %s\n", short_snippet(buffer->ptr, buf, sizeof buf));
    Buffer_free(buffer);
    va_end(argptr);
}

void render_fprintf(FILE* out, char* str, size_t size){
    int res = fprintf(out, "%.*s", (int)size, str);
    guarantee_errno(res >= 0, "fprintf");
}

HEADER_DECLARE
void Term_print(Term* term){
    Term_render(term, RENDER_DEFAULT, MAX_PREC, MAX_PREC, false, (renderer_t)render_fprintf, stdout);
}

HEADER_DECLARE
void do_nothing(){
}
