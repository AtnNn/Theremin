#include "genheader.h"

#include "parse.h"

#if HEADER_ONLY

#include "term.h"

#endif

#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "hashtable.h"
#include "list.h"
#include "debug.h"
#include "error.h"
#include "eval.h"
#include "roots.h"
#include "render.h"
#include "frame.h"

Term* parse_term_vars(char** str, HashTable* vars, char* end_chars);
Term* parse_list(char** str, HashTable* vars);

HEADER_DECLARE
char* spaces(char* str){
    while(true){
        if(isspace(*str)){
            str++;
        }else if(*str == '%'){
            str++;
            while(*str && *str != '\n'){
                str++;
            }
        }else{
            return str;
        }
    }
}

Term* parse_parens(char** str, HashTable* vars){
    char* pos = spaces(*str);
    if(*pos != '('){
        return NULL;
    }
    pos++;
    Term* term = parse_term_vars(&pos, vars, 0);
    if(!term){
        return NULL;
    }
    pos = spaces(pos);
    if(*pos != ')'){
        return NULL;
    }
    *str = pos + 1;
    return term;
}


Term* parse_atomic(char** str, HashTable* vars, bool* maybe_op){
    char* start = spaces(*str);
    char* pos = start;
    char* end;
    bool var = false;
    bool integer = false;
    bool string = false;
    integer_t n;
    if(!*pos){
        return NULL;
    }
    *maybe_op = true;
    if(isalpha(*pos) || *pos == '_'){
        if(isupper(*pos) || *pos == '_'){
            var = true;
        }
        while(isalpha(*pos) || isdigit(*pos) || *pos == '_'){ pos++; }
        end = pos;
    }else if(*pos == '\''){
        *maybe_op = false;
        while(*++pos != '\''){ }
        start++;
        end = pos++;
    }else if(*pos == '\"'){
        string = true;
        while(*++pos != '\"'){ }
        start++;
        end = pos++;
    }else if(issymbol(*pos)){
        while(issymbol(*++pos)){ }
        end = pos;
    }else if(*pos == ','){
        end = ++pos;
    }else if(isdigit(*pos)){
        integer = true;
        n = atoll(pos);
        while(isdigit(*++pos)){ }
        end = pos;
    }else{
        return NULL;
    }
    Term * term;
    if(integer){
        term = Integer(n);
    }else if(string){
        term = String(start, end - start);
    }else{
        const size_t max = 1024;
        size_t len = end - start;
        if(len > max){
            fatal_error("Atomic term longer than 1024 characters");
        }
        char buf[max + 1];
        memcpy(buf, start, len);
        buf[len] = 0;
        if(var){
            if(!strcmp(buf, "_")){
                term = Var(atom_underscore);
            }else{
                Term* var_term = HashTable_get(vars, String_nt(buf));
                if(Var_is_terminal(var_term)){
                    set_var(var_term, Var(intern_nt(buf)));
                }
                term = var_term;
            }
        }else{
            term = Atom(intern_nt(buf));
        }
    }
    *str = pos;
    return term;
}

Term* parse_args(char **str, atom_t atom, HashTable* vars){
    char* pos = spaces(*str);
    if(*pos != '('){
        return NULL;
    }
    pos++;
    Term* list = Nil();
    functor_size_t count = 0;
    while(true){
        if(*pos == ')'){
            pos++;
            break;
        }
        Term* term = parse_term_vars(&pos, vars, ",");
        if(!term){
            return NULL;
        }
        list = Functor2(atom_cons, term, list);
        count++;
        pos = spaces(pos);
        if(*pos == ','){
            pos = spaces(pos + 1);
        }else if(*pos == ')'){
            pos++;
            break;
        }else{
            return NULL;
        }
    }
    Term* functor = Functor_unsafe(atom, count);
    for(functor_size_t i = 0; i < count; i++){
        Functor_set_arg(functor, count - i - 1, List_head(list));
        list = List_tail(list);
    }
    *str = pos;
    return functor;
}

Term* parse_simple_term(char** str, HashTable* vars){
    char* pos = spaces(*str);
    switch(*pos){
    case '(':
        return parse_parens(str, vars);
    case '[':
        return parse_list(str, vars);
    case '{':
        if(*(pos + 1) == '}'){
            break;
        }
        pos++;
        Term* inner = parse_term_vars(&pos, vars, "}");
        if(inner && *pos == '}'){
            *str = pos + 1;
            return Functor1(atom_braces, inner);
        }else{
            return NULL;
        }
    case '.': {
        char next = *(pos + 1);
        if(isspace(next) || !next) return NULL; }
    }
    bool maybe_op = false;
    Term* atom = parse_atomic(&pos, vars, &maybe_op);
    if(!atom) return NULL;
    if(maybe_op && is_Atom(atom) && HashTable_find(root.ops, atom)){
        *str = pos;
        return atom;
    }
    if(is_Atom(atom)){
        Term* functor = parse_args(&pos, atom->data.functor.atom, vars);
        if(functor){
            *str = pos;
            return functor;
        }
    }
    *str = pos;
    return atom;
}

Term* parse_list(char** str, HashTable* vars){
    char* pos = spaces(*str);
    if(*pos != '['){
        return NULL;
    }
    pos++;
    Term* list = NULL;
    Term** rest = &list;
    while(true){
        Term* term = parse_term_vars(&pos, vars, ",|]");
        if(!term){
            pos = spaces(pos);
            if(*pos == ']'){
                *str = pos + 1;
                return Nil();
            }else{
                return NULL;
            }
        }
        *rest = Functor2(atom_cons, term, NULL);
        rest = &(*rest)->data.functor.args[1];
        pos = spaces(pos);
        if(*pos == ','){
            pos++;
        }else if(*pos == '|'){
            pos++;
            term = parse_term_vars(&pos, vars, "]");
            if(!term) return NULL;
            *rest = term;
            pos = spaces(pos);
            if(*pos != ']') return NULL;
            pos++;
            *str = pos;
            return list;
        }else if(*pos == ']'){
            pos++;
            *str = pos;
            *rest = Nil();
            return list;
        }else{
            return NULL;
        }
    }
}

Term* combine_terms(integer_t prec, Term*** terms){
    Term** pos = *terms;
    Term* left_term = NULL;
    while(true){
        if(!*pos){
            D_PARSE{ debug("no more terms to combine\n"); }
            *terms = pos;
            return left_term;
        }
        Term* ret = NULL;
        Term** ret_pos = NULL;
        Term* list = is_Atom(*pos) ? HashTable_find(root.ops, *pos) : NULL;
        if(!list){
            if(!left_term){
                left_term = *pos++;
                if(!*pos){
                    *terms = pos;
                    return left_term;
                }
                list = is_Atom(*pos) ? HashTable_find(root.ops, *pos) : NULL;
                if(!list){
                    *terms = pos;
                    return left_term;
                }
            }else{
                D_PARSE{ trace_term("not an operator", *pos); }
                *terms = pos;
                return left_term;
            }
        }
        atom_t name = (*pos)->data.functor.atom;
        for(; !Var_is_terminal(list); list = chase(List_tail(list))){
            Term* op = List_head(list);
            D_PARSE{ trace_term("trying op", op); }
            Term** args = Functor_get(op, atom_op, 3);
            if(!args || args[0]->type != INTEGER || args[1]->type != FUNCTOR){
                fatal_error("invalid entry in ops table");
            }
            integer_t left_prec, right_prec;
            atom_t op_spec = args[1]->data.functor.atom;
            op_type(args[0]->data.integer, op_spec, &left_prec, &right_prec);
            if(left_prec && !left_term){
                D_PARSE{
                    debug("missing left operand for '%s' '%s'\n",
                            atom_to_string(op_spec)->ptr, atom_to_string(name)->ptr);
                }
                continue;
            }
            if(left_term && !left_prec){
                D_PARSE{
                    debug("extra left operand for '%s' '%s'\n",
                            atom_to_string(op_spec)->ptr, atom_to_string(name)->ptr);
                }
                continue;
            }
            if(left_prec > prec){
                D_PARSE{
                    debug("dropping '%s', too loose (%ld <= %ld)\n",
                            atom_to_string(name)->ptr, left_prec, prec);
                }
                continue;
            }
            Term* right_term = NULL;
            Term** cur = pos + 1;
            if(right_prec){
                right_term = combine_terms(right_prec, &cur);
                if(!right_term){
                    continue;
                }
            }
            if(ret){
                D_PARSE{ debug("rejecting possibly ambiguous parse\n"); }
                return NULL;
            }
            ret = !right_prec ? Functor1(name, left_term) :
                !left_prec ? Functor1(name, right_term) :
                Functor2(name, left_term, right_term);
            ret_pos = cur;
        }
        if(!ret){
            if(left_term){
                *terms = pos;
                return left_term;
            }
            D_PARSE{ debug("nothing to combine\n"); }
            return NULL;
        }
        left_term = ret;
        D_PARSE{ trace_term("combined subterm", left_term); }
        pos = ret_pos;
    }
}


Term* parse_term_vars(char** str, HashTable* vars, char* end_chars){
    char* pos = *str;
    D_PARSE{
        char buf[11];
        debug("parsing substr: %s\n", short_snippet(pos, buf, sizeof buf));
    }
    Term* terms[MAX_NO_PAREN_TERMS + 1];
    size_t i = 0;
    while(true){
        pos = spaces(pos);
        if(end_chars && strchr(end_chars, *pos)){
            break;
        }
        Term* term = parse_simple_term(&pos, vars);
        if(!term){
            break;
        }
        if(i == MAX_NO_PAREN_TERMS){
            fatal_error("Over 1024 non-parenthesized terms in a row");
        }
        terms[i++] = term;
        D_PARSE{
            trace_term("constructed subterm", term);
            char buf[20];
            debug("rest: %s\n", short_snippet(pos, buf, sizeof(buf)));
        }
    }
    if(i == 0){
        return NULL;
    }
    if(i == 1){
        *str = pos;
        return terms[0];
    }
    terms[i] = NULL;
    Term** ref = terms;
    Term* term = combine_terms(1200, &ref);
    if(!term){
        return NULL;
    }
    if(*ref){
        D_PARSE{
            trace_term("partially combined", term);
            trace_term("next was", *ref);
        }
        return NULL;
    }
    D_PARSE{ trace_term("combined term", term); }
    *str = pos;
    return term;
}

HEADER_DECLARE
Term* parse_term_partial(char** str){
    FRAME_ENTER;
    FRAME_LOCAL(vars) = Dict(PARSE_VARS_HASHTABLE_SIZE);
    FRAME_RETURN(Term*, parse_term_vars(str, vars->data.dict, NULL));
}

HEADER_DECLARE
Term* parse_term(char* str){
    D_PARSE{ debug("\nparsing str: %s\n", str); }
    Term* term = parse_term_partial(&str);
    if(*str){
        D_PARSE{ trace_term("partial parse", term); }
        return NULL;
    }
    return term;
}

HEADER_DECLARE
Term* parse_toplevel(char* str){
    FRAME_ENTER;
    char* pos = spaces(str);
    FRAME_LOCAL(list) = Var(atom_underscore);
    FRAME_LOCAL(tail) = list;
    FRAME_LOCAL(term) = NULL;
    for(; *pos; pos = spaces(pos)){
        term = parse_term_partial(&pos);
        if(!term){
            FRAME_RETURN(Term*, NULL);
        }
        pos = spaces(pos);
        if(*pos != '.'){
            FRAME_RETURN(Term*, NULL);
        }
        pos++;
        Var_push(&tail, term);
        SANITY_CHECK;
    }
    set_var(tail, Nil());
    FRAME_RETURN(Term*, list);
}

HEADER_DECLARE
Term* parse_file(char* path){
    FILE* fp = fopen(path, "r");
    if(!fp){
        fatal_error("could not open '%s': %s", path, strerror(errno));
    }
    int res = fseek(fp, 0, SEEK_END); guarantee_errno(res >= 0, "fseek");
    long size = ftell(fp); guarantee_errno(size >= 0, "ftell");
    res = fseek(fp, 0, SEEK_SET); guarantee_errno(res >= 0, "fseek");
    Buffer* data = Buffer_unsafe(size);
    if(size){
        size_t res_size = fread(data->ptr, size, 1, fp); guarantee(res_size == 1, "fread failed");
    }
    res = fclose(fp); guarantee(res >= 0, "fclose");
    Term* list = parse_toplevel(data->ptr);
    Buffer_free(data);
    return list;
}
