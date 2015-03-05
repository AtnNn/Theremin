#include <stdio.h>
#include <inttypes.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>

#define GLOBALS_SIZE 4096
#define POOL_SECTION_SIZE 4096
#define COLLISION_HASHTABLE_SIZE 256
#define DEFAULT_BUFFER_SIZE 1024

#define HASH_INIT 2166136261
#define HASH_PRIME 16777619

#define UNREACHABLE __builtin_unreachable()

typedef uint32_t hash_t;
typedef uint8_t functor_size_t;
typedef int32_t integer_t;
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

Pool* pool = NULL;
HashTable* globals = NULL;
HashTable* vars = NULL;
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

void Term_destroy(Term* term){
    switch(term->type){
    case VAR:
    case MOVED:
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
    pool->sections++;
    pool->terms = realloc(pool->terms, sizeof(Term*) * pool->sections);
    pool->terms[pool->sections-1] = malloc(sizeof(Term) * POOL_SECTION_SIZE);
}

Term* Pool_add_term_expand(Pool* pool){
     if(pool->free >= pool->sections * POOL_SECTION_SIZE){
        Pool_expand(pool);
     }
     pool->free++;
     return &pool->terms[pool->free / POOL_SECTION_SIZE][pool->free % POOL_SECTION_SIZE];
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
        case MOVED:
            break; // impossible
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
        }
    }
}

void Pool_pour_table(HashTable* table, Pool* new){
    for(size_t i = 0; i < table->size; i++){
        Pool_pour(&table->table[i], new);
    }
}

void gc(Pool* pool){
    Pool *new = Pool_new();
    Pool_pour_table(globals, new);
    Pool_pour(&stack, new);
    Pool_pour(&query, new);
    if(next_query){
        Pool_pour(&next_query, new);
    }
    Pool_free(pool);
    Pool_expand(new);
    pool = new;
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
     return Pool_add_term_expand(pool);
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
    Functor_set_arg(term, 2, b);
    Functor_set_arg(term, 3, c);
    return term;
}

Term* Functor4(char* atom, Term* a, Term* b, Term* c, Term* d){
    Term* term = Functor_unsafe(atom, 4);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 2, b);
    Functor_set_arg(term, 3, c);
    Functor_set_arg(term, 4, d);
    return term;
}

Term* Functor5(char* atom, Term* a, Term* b, Term* c, Term* d, Term* e){
    Term* term = Functor_unsafe(atom, 5);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 2, b);
    Functor_set_arg(term, 3, c);
    Functor_set_arg(term, 4, d);
    Functor_set_arg(term, 5, e);
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

        }
        return hash;
    case VAR:
        fatal_error("Cannot hash a variable");
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

Term** Functor_get(Term* term, char* atom, functor_size_t size){
    if(term->type != FUNCTOR ||
       strcmp(term->data.functor.atom, atom) ||
       term->data.functor.size != size){
        return NULL;
    }
    return term->data.functor.args;
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
        sprintf(buf, "%d", term->data.integer);
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

bool Atom_eq(Term* term, char* atom){
    trace_term("Atom_eq term", term);
    if(Functor_get(term, atom, 0)){
        fprintf(stderr, "Atom_eq <- true\n");
        return true;
    }else{
        fprintf(stderr, "Atom_eq <- false\n");
        return false;
    }
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
        fatal_error("Foudn a moved term when comparing terms");
    case VAR:
        return a == b;
    case INTEGER:
        return a->data.integer == b->data.integer;
    case FUNCTOR:
        if(a->data.functor.atom != b->data.functor.atom){
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
    trace_term("Assoc_get assoc", *assoc);
    trace_term("Assoc_get key", key);
    for(Term* list = *assoc; !Atom_eq(list, "[]"); list = List_tail(list)){
        trace_term("Assoc_get list", list);
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

Term* Term_copy_rec(Term* term, HashTable* vars){
    term = chase(term);
    switch(term->type){
    case INTEGER:
        return term;
    case FUNCTOR:
        gc(pool);
        functor_size_t size = term->data.functor.size;
        Term** args = term->data.functor.args;
        Term* copy = Functor_unsafe(term->data.functor.atom, size);
        for(int i = 0; i < size; i++){
            Functor_set_arg(copy, i, Term_copy_rec(args[i], vars));
        }
        return copy;
    case VAR: {
        Term** copy = HashTable_get(vars, term);
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
    HashTable_reset(vars);
    disable_gc();
    Term* copy = Term_copy_rec(term, vars);
    enable_gc();
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
    Term* rules = HashTable_find(globals, term);
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
        fatal_error("no such predicate");
    }
    if(!next_query){
        next_query = Atom("true");
    }
    stack = Functor4("frame", branches, next_query , Atom("[]"), stack);
    next_query = NULL;
    enable_gc();
}

bool stack_next(bool success){
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
            }else{
                *branches = car_cdr[1];
            }
            next_query = car_cdr[0];
            return true;
        }else{
            stack = parent;
            return stack_next(false);
        }
    }
}

prim_t find_prim(char* name, functor_size_t size){
    if(!strcmp(name, "print") && size == 1){
        return prim_print;
    }
    if(!strcmp(name, "fail") && size == 0){
        return prim_fail;
    }
    return NULL;
}

bool eval(){
    bool success = true;
    while(true){
        Term* term = chase(query);
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
            }
            if(!success || !next_query){
                if(!stack_next(success)){
                    return success;
                }
            }
            query = next_query;
            next_query = NULL;
            break; }
        }
    }
}

int main(){
    disable_gc();

    pool = Pool_new();
    globals = HashTable_new(GLOBALS_SIZE);
    vars = HashTable_new(COLLISION_HASHTABLE_SIZE);
    stack = Atom("empty");

    Term* pred = Functor2("foo", Atom("bar"), Atom("baz"));
    HashTable_append(globals, Spec("foo", 2), pred);

    Term* X = Var("X");
    query = Functor2(",",
                     Functor2("foo", Atom("bar"), X),
                     Functor1("print", X));

    enable_gc();

    eval();
}
