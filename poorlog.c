#include <stdio.h>
#include <inttypes.h>
#include <malloc.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>

#define GLOBALS_SIZE 4096
#define POOL_SECTION_SIZE 4096
#define COLLISION_HASHTABLE_SIZE 256

typedef uint32_t hash_t;
typedef uint8_t functor_size_t;
typedef int32_t integer_t;

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

typedef struct {
    size_t sections;
    size_t free;
    Term** terms;
} Pool;

typedef struct {
    size_t size;
    hash_t (*hash)(Term*);
    Term* table[1];
} HashTable;

HashTable* HashTable_new(size_t size, hash_t (*hash)(Term*)){
    HashTable* table = malloc(sizeof(HashTable) + sizeof(Term*) * (size - 1));
    table->size = size;
    table->hash = hash;
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

void fatal_error(char* message){
    fprintf(stderr, "fatal error: %s\n", message);
    exit(1);
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

Term* Functor4(char* atom, Term* a, Term* b, Term* c, Term* d, Term* e){
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
    term->data.ref.name = name;
    term->data.ref.ref = term;
    return term;
}

hash_t hash_functor_spec(char* name, functor_size_t n){
    uint32_t hash = 2166136261;
    for(; *name; name++){
        hash ^= *name;
        hash *= 16777619;
    }
    hash ^= n;
    hash *= 16777619;
    return hash;
}

void HashTable_insert(HashTable* table, Term* term){
    hash_t hash = table->hash(term);
    Term** list = &table->table[hash % table->size];
    Term* tail = *list;
    disable_gc();
    *list = Functor_unsafe(".", 2);
    Functor_set_arg(*list, 0, term);
    if(tail){
        Functor_set_arg(*list, 1, tail);
    }else{
        Functor_set_arg(*list, 1, Atom("[]"));
    }
    enable_gc();
}

Term* HashTable_find_matching(HashTable* table, Term* term){
    return table->table[table->hash(term) % table->size];
}

Term* chase(Term* term){
    while(term->type == VAR && term->data.ref.ref != term){
        term = term->data.ref.ref;
    }
    return term;
}

void prim_print(Term* term){
    term = chase(term);
    switch(term->type){
    case MOVED:
        printf("_MOVED");
        break;
    case VAR:
        printf("%s", term->data.ref.name ? term->data.ref.name : "_");
        break;
    case INTEGER:
        printf("%ud", term->data.integer);
        break;
    case FUNCTOR:
        printf("%s", term->data.functor.atom);
        if(term->data.functor.size){
            printf("(");
            for(int i = 0; i < term->data.functor.size; i++){
                Term_print(term->data.functor.args[i]);
                if(i + 1 < term->data.functor.size){
                    printf(", ");
                }
            }
            printf(")");
        }
        break;
    }
}

bool prim_fail(Term*){
    return false;
}

Term* Functor_get(Term* term, char* atom, functor_size_t size){
    if(term->type != FUNCTOR ||
       strcmp(term->data.functor.atom, atom) ||
       term->data.functor.size != size){
        return NULL;
    }
    return term->data.functor.args;
}

Term* List_head(Term* list){
    Term* args = Functor_get(list, ".", 2);
    if(!args) fatal_error("Not a list");
    return args[0];
}

Term* List_tail(Term* list){
    Term* args = Functor_get(list, ".", 2);
    if(!args) fatal_error("Not a list");
    return args[1];
}

bool Atom_eq(Term* term, char* atom){
    return !!Functor_get(term, atom, 0);
}

Term* Term_copy_rec(Term* term, HashTable* vars){
    disable_gc();
    term = chase(term);
    switch(term->type){
    case INTEGER:
        return term;
    case FUNCTOR:
        functor_size_t size = term->data.functor.size;
        Term* args = term->data.functor.args;
        Term* copy = Functor_unsafe(term->data.functor.atom, size);
        for(int i = 0; i < size; i++){
            Functor_set_arg(copy, i, Term_copy_rec(args[i], vars));
        }
        return copy;
    case VAR:
    case MOVED:
        // TODO
    }
    enable_gc();
}

bool Rule_spec(Term* term, char** name, int* size){
    Term* args = Functor_get(term, ":-", 2);
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

hash_t Rule_hash(Term* term){
    // TODO
}

bool Rule_match(Term* term, char* atom, functor_size_t size){
    // TODO
}

void stack_push(char* name, functor_size_t size, Term* term){
    Term* list = HashTable_find(name, size);
    disable_gc();
    Term* rules = Atom("[]");
    for(; list = List_tail(list); !Atom_eq(list, "[]")){
        term* rule = List_head(list);
        if(Rule_match(rule, name, size)){
            rules = Functor2(".", Term_copy(rule), rules);
        }
    }
    stack = Functor5("frame", term, rules, next_query, Atom("[]"), stack);
    next_query = NULL;
    enable_gc();
}

bool stack_next(bool success){
    // TODO
}

bool (*)(Term*) find_prim(char* name, functor_size_t size){
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
        case FUNCTOR:
            char* atom = term->data.functor.atom;
            functor_size_t size = term->data.functor.size;
            Term* args = term->data.functor.args;
            if(!strcmp(atom, ",") && size == 2){
                disable_gc();
                next_query = next_query ? Functor2(",", args[1], next_query) : args[1];
                query = args[0]; 
                enable_gc();
                continue;
            }
            bool (*prim)(Term*) = find_prim(atom, size);
            if(prim){
                success = prim(args);
            }else{
                stack_push(atom, size, term);
            }
            if(!sucess || !next_query){
                if(!stack_next(success)){
                    return success;
                }
            }
            query = next_query;
            next_query = NULL;
            break;
        }
    }
}

int main(){
    disable_gc++;

    pool = Pool_new();
    globals = HashTable_new(GLOBALS_SIZE, Rule_hash);
    vars = HashTable_new(COLLISION_HASHTABLE_SIZE, hash_ptr);
    stack = Atom("empty");

    Term* pred = Functor2("foo", Atom("bar"), Atom("baz"));
    HashTable_insert(globals, pred);

    Term* X = Var("X");
    query = Functor2(","
                     Functor2("foo", Atom("bar"), X),
                     Functor1("print", X));

    disable_gc--;

    eval();
}
