// TODO: if a variable is not accessible from the undo stack, collapse it in `chase` and `set_var` and don't save it to the stack
// TODO: separate namespace for each module

#include "genheader.h"

#include "eval.h"

#if HEADER_ONLY

#include "term.h"

typedef struct eval_env_t {
    Term* query;
    Term* next_query;
    Term* stack;
    struct eval_env_t* prev_eval;
} eval_env_t;

#endif

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include "utils.h"
#include "frame.h"
#include "assert.h"
#include "debug.h"
#include "error.h"
#include "list.h"
#include "render.h"
#include "term_copy.h"
#include "prim.h"
#include "parse.h"

typedef struct trace_info_t {
    int depth;
    Term* string;
} trace_info_t;

HEADER_DECLARE
eval_env_t* current_eval_env;

static bool enable_trace = false;

HEADER_DECLARE
bool base_loaded = false;

Term* stack_branches(Term* stack){
    Term** args = Functor_get(stack, atom_frame, 4);
    assert(args, "invalid stack frame");
    return args[0];
}

Term* stack_undo_vars(Term* stack){
    Term** args = Functor_get(stack, atom_frame, 4);
    assert(args, "invalid stack frame");
    return args[1];
}

Term* stack_parent(Term* stack){
    Term** args = Functor_get(stack, atom_frame, 4);
    assert(args, "invalid stack frame");
    return args[2];
}

bool stack_cut_barrier(Term* stack){
    Term** args = Functor_get(stack, atom_frame, 4);
    assert(args, "invalid stack frame");
    return Atom_eq(args[3], atom_true);
}

void new_stack_frame(Term** stack, Term* branches, bool cut_barrier){
    FRAME_ENTER_1(branches);
    *stack = Functor4(atom_frame, branches, Nil(), *stack, Atom(cut_barrier ? atom_true : atom_false));
    FRAME_LEAVE;
}

void stack_set_parent(Term** stack, Term* parent){
    FRAME_ENTER_1(parent);
    Term** args = Functor_get(parent, atom_frame, 4);
    if(args && !Atom_eq(args[3], atom_true)){
        args[3] = Functor_get(*stack, atom_frame, 4)[3];
    }
    *stack =  parent;
    FRAME_LEAVE;
}

int stack_depth(eval_env_t* eval){
    int ret = 0;
    for(Term* stack = eval->stack; !Atom_eq(stack, atom_empty); ret++){
        if(Atom_eq(stack, atom_c_land)){
            eval = eval->prev_eval;
            stack = eval->stack;
        }else{
            stack = stack_parent(stack);
        }
    }
    return ret;
}

void trace_record(trace_info_t* trace, Term* query, eval_env_t* eval){
    if(!enable_trace || !base_loaded) return;
    trace->depth = stack_depth(eval);
    Buffer* buffer = Term_show(query, 0);
    trace->string = String(buffer->ptr, buffer->end);
    Buffer_free(buffer);
}

void trace_show(trace_info_t* trace, bool success){
    if(!enable_trace || !base_loaded) return;
    int depth = trace->depth;
    while(depth--){
        debug("   ");
    }
    debug("%s%s\n", success ? "   " : " ~ ",trace->string->data.string.ptr);
}

HEADER_DECLARE
void set_tracing(bool b){
    enable_trace = b;
}

void add_undo_var(Term* stack, Term* var){
    FRAME_ENTER_2(stack, var);
    if(Atom_eq(stack, atom_empty)){
        FRAME_LEAVE;
        return;
    }
    FRAME_LOCAL(undo_vars) = stack_undo_vars(stack);
    if(!Atom_eq(undo_vars, atom_drop)){
        stack->data.functor.args[1] = Functor2(atom_cons, var, undo_vars);
    }
    FRAME_LEAVE;
}

void add_undo_vars(Term* stack, Term* vars){
    FRAME_ENTER_2(stack, vars);
    if(Atom_eq(vars, atom_drop)){
        FRAME_LEAVE;
        return;
    }
    Term** args = Functor_get(stack, atom_frame, 4);
    if(!args){
        FRAME_LEAVE;
        return;
    }
    if(Atom_eq(args[1], atom_drop)){
        FRAME_LEAVE;
        return;
    }
    for(; !Atom_eq(vars, atom_nil); vars = List_tail(vars)){
        stack->data.functor.args[1] = Functor2(atom_cons, List_head(vars), stack->data.functor.args[1]);
    }
    FRAME_LEAVE;
}

void reset_undo_vars(Term* vars){
    FRAME_ENTER_1(vars);
    if(Atom_eq(vars, atom_drop)){
        FRAME_LEAVE;
        return;
    }
    FRAME_LOCAL(var) = NULL;
    for(; !Atom_eq(vars, atom_nil); vars = List_tail(vars)){
        var = List_head(vars);
        assert(var->type == VAR, "cannot reset non-var");
        var->data.var.ref = var;
    }
    FRAME_LEAVE;
}

HEADER_DECLARE
void set_var(Term* a, Term* b){
    assert(a->type == VAR, "not a variable");
    assert(a->data.var.ref == a, "variable is already set");
    char* name = atom_to_string(a->data.var.name)->ptr;
    D_EVAL{
        if(name[0] != '_'){
            trace_term("set_var `%s'", b, name);
        }
    }
    if(enable_trace && base_loaded && current_eval_env && name[0] != '_'){
        disable_gc();
        trace_info_t trace;
        trace_record(&trace, Functor2(atom_eq, a, b), current_eval_env);
        trace_show(&trace, true);
        enable_gc();
    }
    a->data.var.ref = b;
    if(current_eval_env){
        add_undo_var(current_eval_env->stack, a);
    }
}

HEADER_DECLARE
bool unify(Term* a, Term* b){
    a = chase(a);
    b = chase(b);
    if(a->type == VAR){
        set_var(a, b);
        return true;
    }
    if(b->type == VAR){
        set_var(b, a);
        return true;
    }
    if(a->type != b->type){
        return false;
    }
    switch(a->type){
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
            if(!unify(a->data.functor.args[i], b->data.functor.args[i])){
                return false;
            }
        }
        return true;
    case DICT:
        fatal_error("unimplemented: unify dict");
    case MOVED:
        fatal_error("Cannot unify a moved term");
    case VAR:
    default:
        UNREACHABLE;
    }
    UNREACHABLE;
}


void pop_eval_env(bool success){
    FRAME_ENTER;
    FRAME_LOCAL(stack) = current_eval_env->stack;
    current_eval_env = current_eval_env->prev_eval;
    FRAME_LOCAL(next) = NULL;
    D_EVAL{ debug("pop eval: %s\n", success ? "true" : "fail"); }
    while(!Atom_eq(stack, atom_c_land) && !Atom_eq(stack, atom_empty)){
        Term** args = Functor_get(stack, atom_frame, 4);
        assert(args, "invalid stack frame");
        next = args[2];
        D_EVAL{
            trace_term(success ? "pop add variables" : "pop reset variables", args[1]);
        }
        if(success){
            add_undo_vars(current_eval_env->stack, args[1]);
        }else{
            reset_undo_vars(args[1]);
        }
        stack = next;
    }
    FRAME_LEAVE;
}

void error(char* format, ...){
    va_list argptr;
    va_start(argptr, format);
    int res = fprintf(stderr, "error: ");
    guarantee_errno(res >= 0, "fprintf");
    res = vfprintf(stderr, format, argptr);
    guarantee_errno(res >= 0, "vfprintf");
    res = fprintf(stderr, "\n");
    guarantee_errno(res >= 0, "fprintf");
    va_end(argptr);
    return false;
}

bool stack_push(eval_env_t* eval, atom_t atom, functor_size_t size, Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(spec) = Spec(atom, size);
    Term* rules = HashTable_find(root.globals, spec);
    if(!rules){
        error("No such predicate '%s'/%u", atom_to_string(atom)->ptr, size) ;
        FRAME_RETURN(bool, false);
    }
    FRAME_LOCAL(branches) = Var(atom_underscore);
    FRAME_LOCAL(branches_tail) = branches;
    FRAME_LOCAL(var) = NULL;
    FRAME_LOCAL(head) = Nil();
    FRAME_LOCAL(branch) = Nil();
    for(; !Var_is_terminal(rules); rules = chase(List_tail(rules))){
        head = Term_copy(List_head(rules));
        Term** args = Functor_get(head, atom_entails, 2);
        if(args){
            branch = Functor2(atom_comma, Functor2(atom_eq, term, args[0]), args[1]);
        }else{
            branch = Functor2(atom_eq, term, head);
        }
        if(eval->next_query){
            branch = Functor2(atom_comma, branch, eval->next_query);
        }
        var = Var(atom_underscore);
        set_var(branches_tail, Functor2(atom_cons, branch, var));
        branches_tail = var;
    }
    set_var(branches_tail, Nil());
    if(Atom_eq(branches, atom_nil)){
        error("No rules for predicate '%s/%u'", atom_to_string(atom), size) ;
        FRAME_RETURN(bool, false);
    }
    // TODO: make prim
    bool cuttable = !(atom == atom_or);
    new_stack_frame(&eval->stack, branches, cuttable);
    eval->next_query = NULL;
    FRAME_RETURN(bool, true);
}

bool stack_next(eval_env_t* eval, bool success){
    D_EVAL{
        debug("stack_next(%s)\n", success ? "true" : "fail");
        trace_term("stack_next stack", eval->stack);
        if(eval->next_query){
            trace_term("stack_next next_query", eval->next_query);
        }
    }
    if(Atom_eq(eval->stack, atom_empty)){
        return false;
    }
    assert(!Atom_eq(eval->stack, atom_c_land), "missing frame on stack");
    Term** args = Functor_get(eval->stack, atom_frame, 4);
    assert(args, "stack should be empty/0, c_land/0 or frame/3");
    Term** branches = &args[0];
    Term** vars = &args[1];
    Term* parent = args[2];
    if(Atom_eq(parent, atom_c_land)){
        assert(Atom_eq(*branches, atom_true), "first frame not empty");
        return false;
    }
    if(success){
        add_undo_vars(parent, *vars);
        stack_set_parent(&eval->stack, parent);
        eval->next_query = Atom(atom_true);
        return true;
    }else{
        reset_undo_vars(*vars);
        *vars = Nil();
        Term** car_cdr = Functor_get(chase(*branches), atom_cons, 2);
        if(car_cdr){
            Term** cadr_args = Functor_get(car_cdr[1], atom_frame, 4);
            if(Atom_eq(car_cdr[1], atom_nil) ||
               (cadr_args && Atom_eq(cadr_args[1], atom_drop))){
                *vars = Atom(atom_drop);
            }
            *branches = car_cdr[1];
            eval->next_query = car_cdr[0];
            return true;
        }else{
            stack_set_parent(&eval->stack, parent);
            return stack_next(eval, false);
        }
    }
}

void cut(Term* stack){
    FRAME_ENTER_1(stack);
    Term** args;
    while(true){
        args = Functor_get(stack, atom_frame, 4);
        if(!args){
            break;
        }
        args[0] = Atom(atom_true);
        if(Atom_eq(args[3], atom_true)){
            break;
        }
        stack = args[2];
    }
    FRAME_LEAVE;
}

HEADER_DECLARE
bool eval_query(Term* query){
    FRAME_ENTER_1(query);
    FRAME_LOCAL(term) = Nil();
    eval_env_t eval;
    eval.query = NULL; FRAME_TRACK_VAR(eval.query);
    eval.next_query = NULL; FRAME_TRACK_VAR(eval.next_query);
    if(current_eval_env){
        eval.stack = Functor4(atom_frame, Atom(atom_true), Nil(), Atom(atom_c_land), Atom(atom_false));
    }else{
        eval.stack = Atom(atom_empty);
    }
    FRAME_TRACK_VAR(eval.stack);
    eval.prev_eval = current_eval_env;
    current_eval_env = &eval;
    trace_info_t trace;
    trace.string = NULL;
    FRAME_TRACK_VAR(trace.string);
    while(true){
        SANITY_CHECK;
        bool success = true;
        term = chase(query);
        eval.query = term;
        trace_record(&trace, query, &eval);
        switch(term->type){
        case INTEGER:
            pop_eval_env(false);
            error("Cannot eval integer %ld", term->data.integer) ;
            FRAME_RETURN(bool, false);
        case STRING:
            pop_eval_env(false);
            error("Cannot eval string \"%s\"", term->data.string) ;
            FRAME_RETURN(bool, false);
        case VAR:
            pop_eval_env(false);
            error("Cannot eval unbound variable '%s'", atom_to_string(term->data.var.name)) ;
            FRAME_RETURN(bool, false);
        case MOVED:
            fatal_error("Cannot eval moved term");
        case DICT:
            fatal_error("Cannot eval dict term");
        case FUNCTOR: {
            atom_t atom = term->data.functor.atom;
            functor_size_t size = term->data.functor.size;
            Term** args = term->data.functor.args;
            if(atom == atom_comma && size == 2){
                eval.next_query = eval.next_query ? Functor2(atom_comma, args[1], eval.next_query) : args[1];
                query = args[0];
                continue;
            }
            if(atom == atom_cut && size == 0){
                cut(eval.stack);
                query = eval.next_query;
                eval.next_query = NULL;
                continue;
            }
            D_EVAL{
                trace_term("eval term", term);
            }
            prim_t prim = find_prim(atom, size);
            if(prim){
                success = prim(args);
                trace_show(&trace, success);
            }else{
                if(!stack_push(&eval, atom, size, term)){
                    trace_show(&trace, false);
                    pop_eval_env(false);
                    FRAME_RETURN(bool, false);
                }
                trace_show(&trace, true);
                success = false;
            }
            if(!success || !eval.next_query){
                if(!stack_next(&eval, success)){
                    D_EVAL{ trace_term("eval stack", eval.stack); }
                    pop_eval_env(success);
                    FRAME_RETURN(bool, success);
                }
            }
            query = eval.next_query;
            eval.next_query = NULL;
            break; }
        default:
            UNREACHABLE;
        }
    }
}

HEADER_DECLARE
bool assertz(Term* term){
    FRAME_ENTER_1(term);
    Term** args = Functor_get(term, atom_entails, 2);
    if(args){
        FRAME_LOCAL(head) = chase(args[0]);
        if(head->type != FUNCTOR){
            D_EVAL{ trace_term("assertz: not a functor", head); }
            FRAME_RETURN(bool, false);
        }
        HashTable_append(root.globals, Spec(head->data.functor.atom, head->data.functor.size), term);
        FRAME_RETURN(bool, true);
    }
    args = Functor_get(term, atom_long_rarrow, 2);
    if(args){
        FRAME_LOCAL(q) = Functor1(atom_assertz_dcg, term);
        bool ret = eval_query(q);
        FRAME_RETURN(bool, ret);
    }
    HashTable_append(root.globals, Spec(term->data.functor.atom, term->data.functor.size), term);
    FRAME_RETURN(bool, true);
}

HEADER_DECLARE
void load_file(char* path){
    FRAME_ENTER;
    FRAME_LOCAL(contents) = parse_file(path);
    guarantee(contents, "failed to parse file `%s'", path);
    contents = chase(contents);
    for(; !Atom_eq(contents, atom_nil); contents = chase(List_tail(contents))){
        eval_toplevel(List_head(contents));
    }
    FRAME_LEAVE;
}

HEADER_DECLARE
void eval_toplevel(Term* term){
    if(term->type != FUNCTOR){
        trace_term("eval_toplevel term", term);
        fatal_error("toplevel term must be functor");
    }
    Term** args = Functor_get(term, atom_entails, 1);
    if(args){
        if(!eval_query(args[0])){
            trace_term("failed directive", args[0]);
            exit(1);
        }
        return;
    }
    if(!assertz(term)){
        trace_term("top-level term", term);
        fatal_error("failed to assertz term");
    }
}

HEADER_DECLARE
void load_base(){
    load_file(LIB_PATH "/base.pl");

    base_loaded = true;
}
