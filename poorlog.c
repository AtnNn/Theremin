#include <stdio.h>

typedef struct List {
    struct List* next;
    int data;
}

#define List_new(T, list) List_new_impl(list, sizeof(List)-sizeof(int)+sizeof(T))
List* List_new_impl(List* list, size_t n){
    List* new = malloc(n);
    new->next = list;
    return new;
}

List* List(){
    return NULL;
}

#define List_head(T, list) ((T*)&list->data)

#define List_remove(T, list) List_remove_impl(list, T ## _destroy)
void List_remove_impl(List** list, void(*T_destroy)(void*)){
    T_destroy((T*)&list->data);
    List* tmp = *list;
    *list = (*list)->next;
    free(tmp);
}

#define List_free(T, list) List_free_impl(list, T ## _destroy)
void List_free_impl(List* list, void(*T_destroy)(void*)){
    while(list){
        List* next = list->next;
        T_destroy((void*)&list->data);
        free(list);
        list = next;
    }
}

typedef struct {
    List* assignments;
} Frame;

void Frame_destroy(Frame* frame){
    List_free(frame->assignments);
}

typedef struct {
    Term* var;
    Term* val;
} OldValue;

void OldValue_destroy(void*){ }

typedef struct {
    size_t size;
    Term* table[1];
} HashTable;

HashTable* HashTable(size_t size){
    HashTable* table = malloc(sizeof(HashTable) + sizeof(Term) * (size - 1));
    table->size = size;
    memset(table->table, 0, sizeof(Term) * size);
    return table;
}

void HashTable_insert(HashTable* table, uint32_t hash, Term* term){
    
}

#define POOL_SECTION_SIZE 4096;

typedef struct Term {
    enum { FUNCTOR, VAR, MOVED, INTEGER } type;
    union {
        uint32_t integer;
        struct {
            char* atom;
            uint8_t size;
            Term** args;
        } functor;
        struct {
            char* name;
            Term* ref;
        } ref;
    } data;
} Term;

typedef struct {
    size_t sections;
    size_t free;
    Term** terms;
} Pool;

Pool* pool;
Dict* globals;
List* env;

Pool* Pool(){
    Pool* pool = malloc(sizeof(Pool));
    pool->sections = 1;
    pool->free = 0;
    pool->terms = malloc(sizeof(Term*));
    pool->terms[0] = malloc(sizeof(Term) * POOL_SECTION_SIZE);
    return pool;
}

void Term_destroy(Term* term){
    switch(term->type){
    VAR:
    MOVED:
        free(term->data.ref.name);
        break;
    FUNCTOR:
        free(term->data.functor.atom);
        free(term->data.functor.args);
        break;
    INTEGER:
        break;
    }
}

void Pool_free(Pool* pool){
    for(size_t i = 0; i < pool->sections; i++){
        for(size_t j = o; j < POOL_SECTION_SIZE; j++){
            Term_destroy(pool->terms[i][j]);
        }
        free(pool->terms[i]);
    }
    free(pool->terms);
    free(pool);
}

Term* Pool_add_term_expand(Pool* pool){
     if(pool->free >= pool->size * POOL_SECTION_SIZE){
         pool->sections++;
         pool->terms = realloc(pool->terms, sizeof(Term*) * pool->sections);
         pool->terms[pool->sections-1] = malloc(sizeof(Term) * POOL_SECTION_SIZE);
     }
     pool->free++;
     return pool->terms[pool->free / POOL_SECTION_SIZE][pool->free % POOL_SECTION_SIZE];
}

void Pool_pour(Term** term, Pool *pool){
    if((*term)->type == MOVED){
        *term = (*term)->data.ref.ref;
    }else{
        Term* new = add_term_expand(pool);
        memcpy(new, *term, size(Term));
        (*term)->type = MOVED;
        (*term)->data.ref.ref = new;
        *term = new;
        switch(new->type){
        MOVED:
            break; // impossible
        VAR:
            Pool_pour(&new->data.ref.ref, pool);
            break;
        FUNCTOR:
            for(uint8_t i = 0; i < new->data.functor.size; i++){
                Pool_pour(&new->data.functor.args[i], pool);
            }
            break;
        INTEGER:
            break;
        }
    }
}

void Pool_pour_env(List* env, Pool* new){
    for(List* list = env; list = list->next; list){
        for(List* assl = List_head(Frame, list)->assignments; assl = assl->next; assl){
            OldValue* ov = List_head(OldValue, assl);
            Pool_pour(&ov->var, new);
            Pool_pour(&ov->val, new);
        }
    }
}

void Pool_pour_dict(Dict* dict, Pool* new){
    // TODO
}

void gc(Pool* pool){
    size_t pos = 0;
    Pool *new = Pool();
    Pool_pour_env(env, new);
    Pool_pour_dict(globals, new);
    Pool_free(pool);
    pool = new;
}

 erm* add_term_gc(Pool* pool){
     if(pool->free >= pool->size * POOL_SECTION_SIZE){
         gc(pool);
     }
     return add_term_expand(pool);
}

Term* Integer(uint32_t n){
    Term* term = add_term_gc(pool);
    term->type = INTEGER;
    term->data.integer = n
    return term;
}

Term* Atom(char* atom){
    Term* term = add_term_gc(pool);
    term->type = FUNCTOR;
    term->data.functor.atom = strdup(atom);
    term->data.functor.size = 0;
    term->data.functor.args = NULL;
    return term;
}

Term* Functor(char* atom, uint8_t size){
    Term* term = add_term_gc(pool);
    term->type = FUNCTOR;
    term->data.functor.atom = strdup(atom);
    term->data.functor.size = size;
    term->data.functor.args = malloc(sizeof(Term*) * size);
    for(uint8_t i = 0; i < size; i++){
        term->data.functor.args[i] = NULL;
    }
}

Term* Var(struct Term *ref, char* name){
    Term* term = add_term_gc(pool);
    term->type = VAR;
    term->data.ref.name = name;
    term->data.ref.ref = ref;
}

void fatal_error(char* message){
    fprintf(stderr, "fatal error: %s\n", message);
    exit(1);
}

void Functor_set_arg(Term* term, uint8_t n, Term* arg){
    if(term->type != FUNCTOR) fatal_error("Functor_set_arg: not a functor");
    if(term->data.functor.size <= n) fatal_error("Functor_set_arg: too few arguments");
    term->data.functor.args[n] = arg;
}

int main(){
    pool = new_pool();
    globals = Dict();
    stack = List();
    //define_builtins(globals);
}
