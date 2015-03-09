#include <stdio.h>
#include <inttypes.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>

#define DEBUG_EVAL 0
#define DEBUG_PARSE 0
#define DEBUG_GC 0

#define GLOBALS_SIZE 4096
#define POOL_SECTION_SIZE 4096
#define COLLISION_HASHTABLE_SIZE 1024
#define PARSE_VARS_HASHTABLE_SIZE 1024
#define OPS_HASHTABLE_SIZE 1024
#define DEFAULT_BUFFER_SIZE 1024
#define MAX_NO_PAREN_TERMS 1024

#define HASH_INIT 2166136261
#define HASH_PRIME 16777619

#define UNREACHABLE __builtin_unreachable()

#if DEBUG_PARSE
#define D_PARSE if(true)
#else
#define D_PARSE if(false)
#endif

#if DEBUG_EVAL
#define D_EVAL if(true)
#else
#define D_EVAL if(false)
#endif

#if DEBUG_GC
#define D_GC if(true)
#else
#define D_GC if(false)
#endif

typedef uint32_t hash_t;
typedef uint8_t functor_size_t;
typedef int64_t integer_t;
typedef void (*renderer_t)(void*, char*);
typedef struct Term {
    enum { FUNCTOR, VAR, MOVED, INTEGER } type;
    union {
        integer_t integer;
        struct {
            char* atom;
            functor_size_t size;
            struct Term** args;
        } functor;
        struct {
            char* name;
            struct Term* ref;
        } ref;
    } data;
} Term;

typedef bool (*prim_t)(Term**);
typedef hash_t (*hash_function_t)(Term*);

typedef struct {
    size_t size;
    size_t pos;
    char* str;
} Buffer;

typedef struct {
    size_t sections;
    size_t free;
    Term** terms;
} Pool;

typedef struct {
    size_t size;
    Term* table[1];
} HashTable;

#define MIN(a,b) ((a) < (b) ? a : b)
#define MAX(a,b) ((a) < (b) ? b : a)

void trace_term(char* str, Term* term);
Term* parse_term_vars(char** str, HashTable* vars);

Buffer* Buffer_new(size_t size){
    Buffer* buffer = malloc(sizeof(Buffer));
    buffer->size = size;
    buffer->pos = 0;
    buffer->str = malloc(size + 1);
    buffer->str[0] = 0;
    return buffer;
}

void Buffer_free(Buffer* buffer){
    free(buffer->str);
    free(buffer);
}

void Buffer_resize(Buffer* buffer, size_t size){
    buffer->str = realloc(buffer->str, size);
    buffer->size = size;
}

HashTable* HashTable_new(size_t size){
    HashTable* table = malloc(sizeof(HashTable) + sizeof(Term*) * (size - 1));
    table->size = size;
    memset(table->table, 0, sizeof(Term*) * size);
    return table;
}

void HashTable_free(HashTable* table){
    free(table);
}

Pool* pool = NULL;
HashTable* globals = NULL;
HashTable* ops = NULL;
Term* stack = NULL;
Term* query = NULL;
Term* next_query = NULL;
int gc_disable_count = 0;
bool would_gc = false;

void fatal_error(char* format, ...){
    va_list argptr;
    va_start(argptr, format);
    fprintf(stderr, "fatal error: ");
    vfprintf(stderr, format, argptr);
    fprintf(stderr, "\n");
    va_end(argptr);
    exit(1);
    UNREACHABLE;
}

Pool* Pool_new(){
    Pool* pool = malloc(sizeof(Pool));
    pool->sections = 1;
    pool->free = 0;
    pool->terms = malloc(sizeof(Term*));
    pool->terms[0] = malloc(sizeof(Term) * POOL_SECTION_SIZE);
    return pool;
}

void trace_pool_info(char* str, Pool* pool){
    fprintf(stderr, "%s: %zu used, %zu available in %zu sections%s\n",
            str,  pool->free, pool->sections * POOL_SECTION_SIZE,
            pool->sections, would_gc ? " (would gc)" :"");
}

void Term_destroy(Term* term){
    switch(term->type){
    case MOVED:
        return;
    case VAR:
        free(term->data.ref.name);
        break;
    case FUNCTOR:
        free(term->data.functor.atom);
        free(term->data.functor.args);
        break;
    case INTEGER:
        break;
    }
}

void Pool_free(Pool* pool){
    D_GC{ trace_pool_info("freeing", pool); }
    for(size_t i = 0; i < pool->sections; i++){
        for(size_t j = 0; j < POOL_SECTION_SIZE; j++){
            Term_destroy(&pool->terms[i][j]);
        }
        free(pool->terms[i]);
    }
    free(pool->terms);
    free(pool);
}

void Pool_expand(Pool* pool){
    D_GC{ trace_pool_info("expanding", pool); }
    pool->sections++;
    pool->terms = realloc(pool->terms, sizeof(Term*) * pool->sections);
    pool->terms[pool->sections-1] = malloc(sizeof(Term) * POOL_SECTION_SIZE);
    D_GC{ trace_pool_info("expanded", pool); }
}

Term* Pool_add_term_expand(Pool* pool){
     if(pool->free >= pool->sections * POOL_SECTION_SIZE){
        Pool_expand(pool);
     }
     Term* term = &pool->terms[pool->free / POOL_SECTION_SIZE][pool->free % POOL_SECTION_SIZE];
     pool->free++;
     return term;
}

void Pool_pour(Term** term, Pool *pool){
    if((*term)->type == MOVED){
        *term = (*term)->data.ref.ref;
    }else{
        Term* new = Pool_add_term_expand(pool);
        memcpy(new, *term, sizeof(Term));
        (*term)->type = MOVED;
        (*term)->data.ref.ref = new;
        *term = new;
        switch(new->type){
        case VAR:
            Pool_pour(&new->data.ref.ref, pool);
            break;
        case FUNCTOR:
            for(functor_size_t i = 0; i < new->data.functor.size; i++){
                Pool_pour(&new->data.functor.args[i], pool);
            }
            break;
        case INTEGER:
            break;
        case MOVED:
            UNREACHABLE;
        }
    }
}

void Pool_pour_table(HashTable* table, Pool* new){
    for(size_t i = 0; i < table->size; i++){
        if(table->table[i]){
            Pool_pour(&table->table[i], new);
        }
    }
}

void gc(Pool* pool){
    D_GC{ trace_pool_info("start gc", pool); }
    Pool *new = Pool_new();
    D_GC{ trace_pool_info("new pool", new); }
    Pool_pour_table(globals, new);
    Pool_pour_table(ops, new);
    Pool_pour(&stack, new);
    Pool_pour(&query, new);
    if(next_query){
        Pool_pour(&next_query, new);
    }
    Pool_free(pool);
    Pool_expand(new);
    pool = new;
    D_GC{ trace_pool_info("finished gc", pool); }
}

void disable_gc(){
    gc_disable_count++;
}

void enable_gc(){
    gc_disable_count--;
    if(would_gc && gc_disable_count == 0){
        gc(pool);
        would_gc = false;
    }
}

Term* Pool_add_term_gc(Pool* pool){
    if(pool->free >= pool->sections * POOL_SECTION_SIZE){
        if(gc_disable_count){
            would_gc = true;
        }else{
            gc(pool);
        }
     }
     Term* term = Pool_add_term_expand(pool);
     return term;
}

Term* Integer(integer_t n){
    Term* term = Pool_add_term_gc(pool);
    term->type = INTEGER;
    term->data.integer = n;
    return term;
}

Term* Atom(char* atom){
    Term* term = Pool_add_term_gc(pool);
    term->type = FUNCTOR;
    term->data.functor.atom = strdup(atom);
    term->data.functor.size = 0;
    term->data.functor.args = NULL;
    return term;
}

Term* Functor_unsafe(char* atom, functor_size_t size){
    Term* term = Pool_add_term_gc(pool);
    term->type = FUNCTOR;
    term->data.functor.atom = strdup(atom);
    term->data.functor.size = size;
    term->data.functor.args = malloc(sizeof(Term*) * size);
    for(functor_size_t i = 0; i < size; i++){
        term->data.functor.args[i] = NULL;
    }
    return term;
}

void Functor_set_arg(Term* term, functor_size_t n, Term* arg){
    if(term->type != FUNCTOR) fatal_error("Functor_set_arg: not a functor");
    if(term->data.functor.size <= n) fatal_error("Functor_set_arg: too few arguments");
    term->data.functor.args[n] = arg;
}

Term* Functor1(char* atom, Term* a){
    Term* term = Functor_unsafe(atom, 1);
    Functor_set_arg(term, 0, a);
    return term;
}

Term* Functor2(char* atom, Term* a, Term* b){
    Term* term = Functor_unsafe(atom, 2);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    return term;
}

Term* Functor3(char* atom, Term* a, Term* b, Term* c){
    Term* term = Functor_unsafe(atom, 3);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    return term;
}

Term* Functor4(char* atom, Term* a, Term* b, Term* c, Term* d){
    Term* term = Functor_unsafe(atom, 4);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    return term;
}

Term* Functor5(char* atom, Term* a, Term* b, Term* c, Term* d, Term* e){
    Term* term = Functor_unsafe(atom, 5);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    Functor_set_arg(term, 4, e);
    return term;
}

Term* Var(char* name){
    Term* term = Pool_add_term_gc(pool);
    term->type = VAR;
    term->data.ref.name = strdup(name);
    term->data.ref.ref = term;
    return term;
}

hash_t hash_byte(uint8_t c, hash_t hash){
    return (hash ^ c) * HASH_PRIME;
}

hash_t hash_string(char* str, hash_t hash){
    for(; *str; str++){
        hash = hash_byte(*str, hash);
    }
    return hash;
}

hash_t hash_integer(integer_t x, hash_t hash){
    char* c = (char*)&x;
    for(int i = 0; i < sizeof(x); i++){
        hash = hash_byte(c[i], hash);
    }
    return hash;
}

hash_t hash_rec(Term* term, hash_t hash){
    switch(term->type){
    case INTEGER:
        return hash_integer(term->data.integer, hash);
    case FUNCTOR:
        hash = hash_string(term->data.functor.atom, hash);
        functor_size_t size = term->data.functor.size;
        if(size){
            hash = hash_byte(size, hash);
            for(functor_size_t i = 0; i < size; i++){
                hash = hash_rec(term->data.functor.args[i], hash);
            }
        }
        return hash;
    case VAR:
        fatal_error("Cannot hash variable '%s'", term->data.ref.name);
        return 0;
    case MOVED:
        fatal_error("Cannot hash a moved term");
        return 0;
    }
}

hash_t hash(Term* term){
    uint32_t hash = HASH_INIT;
    return hash_rec(term, hash);
}

Term* Spec(char* atom, int size){
    disable_gc();
    return Functor2("/", Atom(atom), Integer(size));
    enable_gc();
}

void Buffer_append(Buffer* buffer, char* str){
    size_t len = strlen(str);
    if(buffer->pos + len >= buffer->size){
        Buffer_resize(buffer, MAX(buffer->size * 2, buffer->pos + len));
    }
    strcpy(buffer->str + buffer->pos, str);
    buffer->pos += len;
}

Term* chase(Term* term){
    while(term->type == VAR && term->data.ref.ref != term){
        term = term->data.ref.ref;
    }
    return term;
}

void Term_render(Term* term, void(*write)(void*, char*), void* data){
    if(!term){
        write(data, "NULL");
        return;
    }
    term = chase(term);
    switch(term->type){
    case MOVED:
        write(data, "_MOVED");
        break;
    case VAR:
        write(data, term->data.ref.name ? term->data.ref.name : "_");
        break;
    case INTEGER: {
        char buf[16];
        sprintf(buf, "%ld", term->data.integer);
        write(data, buf);
        break;
    }
    case FUNCTOR:
        write(data, term->data.functor.atom);
        if(term->data.functor.size){
            write(data, "(");
            for(int i = 0; i < term->data.functor.size; i++){
                Term_render(term->data.functor.args[i], write, data);
                if(i + 1 < term->data.functor.size){
                    write(data, ", ");
                }
            }
            write(data, ")");
        }
        break;
    }
}

Buffer* Term_show(Term* term){
    Buffer* buffer = Buffer_new(DEFAULT_BUFFER_SIZE);
    Term_render(term, (renderer_t)Buffer_append, buffer);
    Buffer_resize(buffer, buffer->pos + 1);
    return buffer;
}

void trace_term(char* str, Term* term){
    Buffer* buffer = Term_show(term);
    fprintf(stderr, "%s: %s\n", str, buffer->str);
    Buffer_free(buffer);
}

Term** Functor_get(Term* term, char* atom, functor_size_t size){
    if(term->type != FUNCTOR ||
       strcmp(term->data.functor.atom, atom) ||
       term->data.functor.size != size){
        return NULL;
    }
    return term->data.functor.args;
}

bool is_Atom(Term* term){
    return term->type == FUNCTOR && term->data.functor.size == 0;
}

bool Atom_eq(Term* term, char* atom){
    if(term->type != FUNCTOR ||
       strcmp(term->data.functor.atom, atom) ||
       term->data.functor.size != 0){
        return false;
    }
    return true;
}

Term* List_head(Term* list){
    Term** args = Functor_get(list, ".", 2);
    if(!args){
        trace_term("list", list);
        fatal_error("Expected non-empty list");
    }
    return args[0];
}

Term* List_tail(Term* list){
    Term** args = Functor_get(list, ".", 2);
    if(!args){
        trace_term("list", list);
        fatal_error("Expected non-empty list");
    }
    return args[1];
}

bool Term_exact_eq(Term* a, Term* b){
    a = chase(a);
    b = chase(b);
    if(a->type != b->type){
        return false;
    }
    switch(a->type){
    case MOVED:
        fatal_error("Found a moved term when comparing terms");
    case VAR:
        return a == b;
    case INTEGER:
        return a->data.integer == b->data.integer;
    case FUNCTOR:
        if(strcmp(a->data.functor.atom, b->data.functor.atom)){
            return false;
        }
        if(a->data.functor.size != b->data.functor.size){
            return false;
        }
        for(int i = 0; i < a->data.functor.size; i++){
            if(!Term_exact_eq(a->data.functor.args[i], b->data.functor.args[i])){
                return false;
            }
        }
        return true;
    }
}

Term** Assoc_get(Term** assoc, Term* key){
    for(Term* list = *assoc; !Atom_eq(list, "[]"); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), ":", 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            return &args[1]; 
        }
    }
    enable_gc();
    Term* pair = Functor2(":", key, NULL);
    *assoc = Functor2(".", pair, *assoc);
    disable_gc();
    return &pair->data.functor.args[1];
}

Term* Assoc_find(Term* assoc, Term* key){
    for(Term* list = assoc; !Atom_eq(list, "[]"); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), ":", 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            return args[1];
        }
    }
    return NULL;
}

Term** HashTable_get(HashTable* table, Term* key){
    Term** assoc = &table->table[hash(key) % table->size];
    disable_gc();
    if(!*assoc){
        *assoc = Atom("[]");
    }
    Term** val = Assoc_get(assoc, key);
    enable_gc();
    return val;
}

void HashTable_append(HashTable* table, Term* key, Term* val){
    Term** list = HashTable_get(table, key);
    disable_gc();
    (*list) = Functor2(".", val, *list ? *list : Atom("[]"));
    enable_gc();
}

Term* HashTable_find(HashTable* table, Term* key){
    Term *assoc = table->table[hash(key) % table->size];
    if(!assoc){
        return NULL;
    }
    return Assoc_find(assoc, key);
}

void render_fprintf(FILE* out, char* str){
    fprintf(out, "%s", str);
}

void Term_print(Term* term){
    Term_render(term, (renderer_t)render_fprintf, stdout);
}

bool prim_print(Term** args){
    Term_print(args[0]);
    return true;
}

bool prim_fail(Term** args){
    return false;
}

bool prim_true(Term** args){
    return true;
}

Term* Term_copy_rec(Term* term, HashTable* vars){
    term = chase(term);
    switch(term->type){
    case INTEGER:
        return term;
    case FUNCTOR:{
        functor_size_t size = term->data.functor.size;
        Term** args = term->data.functor.args;
        Term* copy = Functor_unsafe(term->data.functor.atom, size);
        for(int i = 0; i < size; i++){
            Functor_set_arg(copy, i, Term_copy_rec(args[i], vars));
        }
        return copy;
    }
    case VAR: {
        Term** copy = HashTable_get(vars, Integer((integer_t)term));
        if(!*copy){
            *copy = Var(term->data.ref.name);
        }
        return *copy;
    }
    case MOVED:
        fatal_error("Cannot copy a moved term"); 
        return NULL;
    }
}

void HashTable_reset(HashTable* table){
    memset(table->table, 0, sizeof(Term*) * table->size);
}

Term* Term_copy(Term* term){
    HashTable* vars = HashTable_new(COLLISION_HASHTABLE_SIZE);
    disable_gc();
    Term* copy = Term_copy_rec(term, vars);
    enable_gc();
    HashTable_free(vars);
    return copy;
}

bool Rule_spec(Term* term, char** name, int* size){
    Term** args = Functor_get(term, ":-", 2);
    if(args){
        term = args[0];
    }
    if(term->type == FUNCTOR){
        (*name) = term->data.functor.atom;
        (*size) = term->data.functor.size;
        return true;
    }
    return false;
}

void add_undo_var(Term* var){
    Term** args = Functor_get(stack, "frame", 4);
    if(!args) return;
    Term** undo_vars = &args[2];
    *undo_vars = Functor2(".", var, *undo_vars);
}

void add_undo_vars(Term* vars){
    Term** args = Functor_get(stack, "frame", 4);
    if(!args) return;
    Term** undo_vars = &args[2];
    for(; !Atom_eq(vars, "[]"); vars = List_tail(vars)){
        Term* var = List_head(vars);
        *undo_vars = Functor2(".", var, *undo_vars);
    }
}

void reset_undo_vars(Term* vars){
    for(; !Atom_eq(vars, "[]"); vars = List_tail(vars)){
        Term* var = List_head(vars);
        if(var->type != VAR) fatal_error("cannot reset non-var");
        var->data.ref.ref = var;
    }
}

void stack_push(char* name, functor_size_t size, Term* term){
    disable_gc();
    Term* rules = HashTable_find(globals, Spec(name, size));
    enable_gc();
    if(!rules){
        fatal_error("No such predicate '%s/%u'", name, size);
    }
    disable_gc();
    Term* branches = Atom("[]");
    for(; !Atom_eq(rules, "[]"); rules = List_tail(rules)){
        Term* head = Term_copy(List_head(rules));
        Term* branch;
        Term** args = Functor_get(head, ":-", 2);
        if(args){
            branch = Functor2(",", Functor2("=", term, args[0]), args[1]);
        }else{
            branch = Functor2("=", term, head);
        }
        branches = Functor2(".", branch, branches);
    }
    if(Atom_eq(branches, "[]")){
        fatal_error("No rules for predicate '%s/%u'", name, size);
    }
    if(!next_query){
        next_query = Atom("true");
    }
    stack = Functor4("frame", branches, next_query , Atom("[]"), stack);
    next_query = NULL;
    enable_gc();
}

bool stack_next(bool success){
    D_EVAL{
        fprintf(stderr, "stack_next(%d)\n", success);
        trace_term("stack_next stack", stack);
        trace_term("stack_next next_query",
                   next_query ? next_query : Var("NULL"));
    }
    if(Atom_eq(stack, "empty")){
        return false;
    }
    Term** args = Functor_get(stack, "frame", 4);
    if(!args){
        fatal_error("stack should be empty/0 or frame/4");
    }
    Term** branches = &args[0];
    Term* saved_next_query = args[1];
    Term** vars = &args[2];
    Term* parent = args[3];
    if(success){
        disable_gc();
        stack = parent;
        add_undo_vars(*vars);
        enable_gc();
        next_query = saved_next_query;
        return true;
    }else{
        reset_undo_vars(*vars);
        *vars = Atom("[]");
        Term** car_cdr = Functor_get(*branches, ".", 2);
        if(car_cdr){
            if(Atom_eq(car_cdr[1], "[]")){
                stack = parent; 
                next_query = Functor2(",", car_cdr[0], saved_next_query);
            }else{
                *branches = car_cdr[1];
                next_query = car_cdr[0];
            }
            return true;
        }else{
            stack = parent;
            return stack_next(false);
        }
    }
}

void set_var(Term* a, Term* b){
    if(a->type != VAR) fatal_error("Called set_var on non-var");
    if(a->data.ref.ref != a) fatal_error("Cannot overwrite variable's value");
    a->data.ref.ref = b;
    add_undo_var(a);
}

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
    case FUNCTOR:
        if(strcmp(a->data.functor.atom, b->data.functor.atom)){
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
    case MOVED:
        fatal_error("Cannot unify a moved term");
    case VAR:
        UNREACHABLE;
    }
}

bool prim_unify(Term** args){
    return unify(args[0], args[1]);
}

bool prim_nl(Term** args){
    printf("\n");
    return true;
}

prim_t find_prim(char* name, functor_size_t size){

#define PRIM(f, n, r) if(!strcmp(name, #f) && size == n){ return prim_ ## r; }
    PRIM(print, 1, print);
    PRIM(fail, 0, fail);
    PRIM(true, 0, true);
    PRIM(=, 2, unify);
    PRIM(nl, 0, nl);
#undef PRIM

    return NULL;
}

bool eval(){
    while(true){
        bool success = true;
        Term* term = chase(query);
        D_EVAL{
            trace_term("eval stack", stack);
            trace_term("eval term", term);
        }
        switch(term->type){
        case INTEGER:
            fatal_error("Cannot eval integer");
            break;
        case VAR:
            fatal_error("Cannot eval unbound variable");
            break;
        case MOVED:
            fatal_error("Cannot eval moved term");
            break;
        case FUNCTOR: {
            char* atom = term->data.functor.atom;
            functor_size_t size = term->data.functor.size;
            Term** args = term->data.functor.args;
            if(!strcmp(atom, ",") && size == 2){
                disable_gc();
                next_query = next_query ? Functor2(",", args[1], next_query) : args[1];
                query = args[0]; 
                enable_gc();
                continue;
            }
            prim_t prim = find_prim(atom, size);
            if(prim){
                success = prim(args);
            }else{
                stack_push(atom, size, term);
                success = false;
            }
            if(!success || !next_query){
                if(!stack_next(success)){
                    D_EVAL{ trace_term("eval stack", stack); }
                    return success;
                }
            }
            query = next_query;
            next_query = NULL;
            break; }
        }
    }
}

bool issymbol(char c){
    return !isalnum(c) && !isspace(c) && !strchr("(),'._", c) && isprint(c);
}

char* spaces(char* str){
    while(isspace(*str)){
        str++;
    }
    return str;
}

Term* parse_args(char **str, char* name, HashTable* vars){
    char* pos = spaces(*str);
    if(*pos != '('){
        return NULL;
    }
    pos++;
    Term* list = Atom("[]");
    functor_size_t count = 0;
    while(true){
        Term* term = parse_term_vars(&pos, vars);
        if(!term){
            return NULL;
        }
        list = Functor2(".", term, list);
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
    Term* functor = Functor_unsafe(name, count);
    for(functor_size_t i = 0; i < count; i++){
        Functor_set_arg(functor, count - i - 1, List_head(list));
        list = List_tail(list);
    }
    *str = pos;
    return functor;
}

Term* parse_atomic(char** str, HashTable* vars){
    char* start = spaces(*str);
    char* pos = start;
    char* end;
    bool var = false;
    bool integer = false;
    integer_t n;
    if(!*pos){
        return NULL;
    }
    if(isalpha(*pos) || *pos == '_'){
        if(isupper(*pos) || *pos == '_'){
            var = true;
        }
        while(isalpha(*pos) || *pos == '_'){ pos++; }
        end = pos;
    }else if(*pos == '\''){
        while(*++pos != '\''){ }
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
                term = Var("_");
            }else{
                Term** var_term = HashTable_get(vars, Atom(buf));
                if(!*var_term){
                    *var_term = Var(buf);
                }
                term = *var_term;
            }
        }else{
            term = Atom(buf);
        }
    }
    *str = pos;
    return term;
}

Term* parse_parens(char** str, HashTable* vars){
    char* pos = spaces(*str);
    if(*pos != '('){
        return NULL;
    }
    pos++;
    Term* term = parse_term_vars(&pos, vars);
    if(!term){
        return NULL;
    }
    pos = spaces(pos);
    if(*pos != ')'){
        return NULL;
    }
    *str = pos + 1;
    D_PARSE{ trace_term("parsed parens", term); }
    return term;
}

Term* parse_simple_term(char** str, HashTable* vars){
    char* pos = *str;
    Term* atom = parse_atomic(&pos, vars);
    if(!atom){
        return parse_parens(str, vars);
    }
    if(atom->type == FUNCTOR && atom->data.functor.size == 0){
        Term* functor = parse_args(&pos, atom->data.functor.atom, vars);
        if(functor){
            D_PARSE{ trace_term("parsed functor", functor); }
            *str = pos;
            return functor;
        }
    }
    D_PARSE{ trace_term("parsed atomic", atom); }
    *str = pos;
    return atom;
}

void op_type(integer_t prec, char* spec, integer_t *left, integer_t *right){
    char *r = spec + 2;
    switch(spec[0]){
    case 'x': *left = prec; break;
    case 'y': *left = prec - 1; break;
    case 'f': *left = 0; r = spec + 1; break;
    default: fatal_error("invalid op spec '%s'", spec);
    }
    switch(*r){
    case 'x': *right = prec; break;
    case 'y': *right = prec - 1; break;
    case 0: *right = 0;
    default: fatal_error("invalid op spec '%s'", spec);
    }
}

Term* combine_terms(integer_t prec, Term*** terms){
    Term** pos = *terms;
    if(!*pos){
        return NULL;
    }
    Term* left_term = NULL;
    while(true){
        if(!*pos){
            D_PARSE{ trace_term("combined", left_term); }
            *terms = pos;
            return left_term;
        }
        Term* ret = NULL;
        Term** ret_pos = NULL;
        integer_t ret_right_prec = 0;
        Term* list = is_Atom(*pos) ? HashTable_find(ops, *pos) : NULL;
        if(!list){
            if(!left_term){
                left_term = *pos++;
                if(!*pos){
                    D_PARSE{ trace_term("combined", left_term); }
                    *terms = pos;
                    return left_term;
                }
                list = is_Atom(*pos) ? HashTable_find(ops, *pos) : NULL;
                if(!list){
                    return NULL;
                }
            }else{
                D_PARSE{ trace_term("combined", left_term); }
                *terms = pos;
                return left_term;
            }
        }
        char* name = (*pos)->data.functor.atom;
        for(; !Atom_eq(list, "[]"); list = List_tail(list)){
            Term* op = List_head(list);
            Term** args = Functor_get(op, "op", 3);
            if(!args || !args[0]->type == INTEGER || args[1]->type != FUNCTOR){
                fatal_error("invalid entry in ops table");
            }
            integer_t left_prec, right_prec;
            char* op_spec = args[1]->data.functor.atom;
            op_type(args[0]->data.integer, op_spec, &left_prec, &right_prec);
            if((left_prec && !left_term) || (left_term && !left_prec)){
                continue;
            }
            if(left_prec > prec){
                continue;
            }
            Term* right_term = NULL;
            Term** cur = pos + 1; 
            if(right_prec){
                right_term = combine_terms(right_prec, &cur);
                D_PARSE{ trace_term("peek right term", right_term); }
                if(!right_term){
                    continue;
                }
            }
            if(ret){
                D_PARSE{ fprintf(stderr, "rejecting possibly ambiguous parse\n"); }
                return NULL; // Reject possibly ambiguous parse
            }
            ret = !right_prec ? Functor1(name, left_term) :
                !left_prec ? Functor1(name, right_term) :
                Functor2(name, left_term, right_term);
            ret_pos = cur;
            ret_right_prec = right_prec;
        }
        if(!ret){
            if(left_term){
                D_PARSE{ trace_term("combined", left_term); }
                *terms = pos;
                return left_term;
            }
            return NULL;
        }
        left_term = ret;
        D_PARSE{ trace_term("partial combined", left_term); }
        pos = ret_pos;
    }
}

Term* parse_term_vars(char** str, HashTable* vars){
    char* pos = *str;
    Term* terms[MAX_NO_PAREN_TERMS + 1];
    size_t i = 0;
    while(true){
        pos = spaces(pos);
        Term* term = parse_simple_term(&pos, vars);
        if(!term){
            break;
        }
        if(i == MAX_NO_PAREN_TERMS){
            fatal_error("Over 1024 non-parenthesized terms and operators");
        }
        terms[i++] = term; 
    }
    if(i == 0){
        return NULL;
    }
    terms[i] = NULL;
    Term** ref = terms;
    Term* term = combine_terms(1200, &ref);
    if(!term || *ref){
        return NULL;
    }
    *str = pos;
    return term;
}

Term* parse_term_partial(char** str){
    HashTable* vars = HashTable_new(PARSE_VARS_HASHTABLE_SIZE);
    disable_gc();
    Term* term = parse_term_vars(str, vars);
    enable_gc();
    HashTable_free(vars);
    return term;
}

Term* parse_term(char* str){
    Term* term = parse_term_partial(&str);
    if(*str){
        D_PARSE{ trace_term("partial parse", term); }
        return NULL;
    }
    return term;
}

Term* parse_toplevel(char* str){
    char* pos = spaces(str);
    disable_gc();
    Term* list = Atom("[]");
    for(; *pos; pos = spaces(pos)){
        Term* term = parse_term_partial(&pos);
        if(!term){
            return NULL;
        }
        pos = spaces(pos);
        if(*pos != '.'){
            return NULL;
        } 
        list = Functor2(".", term, list);
    }
    enable_gc();
    return list;
}

void eval_toplevel(Term* term){
    if(term->type != FUNCTOR){
        trace_term("eval_toplevel term", term);
        fatal_error("toplevel term must be functor");
    }
    Term** args = Functor_get(term, ":-", 1);
    if(args){
        query = args[0];
        if(!eval()){
            trace_term("directive", query);
            fatal_error("failed directive");
        }
        return;
    }
    args = Functor_get(term, ":-", 2);
    if(args){
        if(args[0]->type != FUNCTOR){
            trace_term("left of :-", term);
            fatal_error("left of :- must be functor");
        } 
        HashTable_append(globals, Spec(args[0]->data.functor.atom, args[0]->data.functor.size), term);
        return;
    }
    HashTable_append(globals, Spec(term->data.functor.atom, term->data.functor.size), term);
}

void define_stdlib(){
    disable_gc();

#define ADD_OP(prec, order, name) HashTable_append(ops, Atom(name), Functor3("op", Integer(prec), Atom(order), Atom(name)));
    ADD_OP(1200, "xfx", ":-");
    ADD_OP(1200, "xfx", "-->");
    ADD_OP(1200, "fx", ":-");
    ADD_OP(1100, "xfy", ";");
    ADD_OP(1000, "xfy", ",");
    ADD_OP(700, "xfx", "=");
#undef ADD_OP

    eval_toplevel(parse_term("A ; _ :- A"));
    eval_toplevel(parse_term("_ ; B :- B"));

    enable_gc();
}

int main(){
    disable_gc();

    pool = Pool_new();
    globals = HashTable_new(GLOBALS_SIZE);
    ops = HashTable_new(OPS_HASHTABLE_SIZE);
    stack = Atom("empty");

    define_stdlib();

    Term* X = Var("X");
    query = Functor2(",",
                     Functor2(";",
                              Functor2(",",
                                       Functor2("=", X, Integer(1)),
                                       Functor2(",",
                                                Functor1("print", X),
                                                Atom("fail"))),
                              Functor2("=", X, Integer(2))),
                     Functor2(",",
                              Functor1("print", X),
                              Atom("nl")));

    char* str = "(X=1, print(X), fail; X=2), print(X)";
    query = parse_term(str);

    trace_term("query", query);

    enable_gc();

    eval();
}
