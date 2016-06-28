#include <stdio.h>
#include <inttypes.h>
#include <malloc.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>

#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
int kill(pid_t, int);

#define PRELUDE_PATH "boot/stdlib/prelude.pl"

#define GLOBALS_SIZE 4096
#define POOL_SECTION_SIZE 4096
#define INTERNED_TABLE_SIZE 4096
#define COLLISION_HASHTABLE_SIZE 1024
#define PARSE_VARS_HASHTABLE_SIZE 1024
#define OPS_HASHTABLE_SIZE 1024
#define DEFAULT_BUFFER_SIZE 1024
#define MAX_NO_PAREN_TERMS 1024
#define MAX_STREAMS 256

#define HASH_INIT 2166136261
#define HASH_PRIME 16777619

#define UNREACHABLE __builtin_unreachable()

#define D_PARSE if(debug_parse && *debug_enabled)
#define D_EVAL if(debug_eval && *debug_enabled)
#define D_GC if(debug_gc && *debug_enabled)
#define D_HASHTABLE if(debug_hashtable && *debug_enabled)
#define D_ATOM if(debug_atom && *debug_enabled)

typedef uint32_t hash_t;
typedef uint8_t functor_size_t;
typedef int64_t integer_t;
typedef uint64_t atom_t;
typedef void (*renderer_t)(void*, char*, size_t);

typedef struct {
    char* ptr;
    size_t size;
} string_t;

typedef struct Term {
    enum { FUNCTOR, VAR, MOVED, INTEGER, STRING } type;
    union {
        integer_t integer;
        string_t string;
        struct Term* moved_to;
        struct {
            atom_t atom;
            functor_size_t size;
            struct Term** args;
        } functor;
        struct {
            atom_t name;
            struct Term* ref;
        } var;
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

typedef struct {
    int fd;
    char* buf;
    size_t pos;
    size_t size;
    size_t alloc_size;
} Stream;

#define MIN(a,b) ((a) < (b) ? a : b)
#define MAX(a,b) ((a) < (b) ? b : a)

void trace_term(char* str, Term* term, ...);
Term* parse_term_vars(char** str, HashTable* vars, char* end_char);
bool assertz(Term* term);
Term** HashTable_get(HashTable* table, Term* key);
Term* HashTable_find(HashTable* table, Term* key);
void fatal_error(char* format, ...);

Pool* pool = NULL;
HashTable* globals = NULL;
HashTable* ops = NULL;
HashTable* interned = NULL;
HashTable* atom_names = NULL;
atom_t next_free_atom;
Term* stack = NULL;
Term* query = NULL;
Term* next_query = NULL;
Term* keep = NULL;
int gc_disable_count = 0;
bool would_gc = false;
bool prelude_loaded = false;
Stream streams[MAX_STREAMS];
int free_stream = 0;

atom_t atom_slash, atom_colon, atom_nil, atom_cons, atom_op, atom_entails,
    atom_frame, atom_drop, atom_comma, atom_eq, atom_empty, atom_true,
    atom_underscore, atom_assertz_dcg, atom_rarrow;

atom_t atom_is, atom_add;

atom_t atom_process_create, atom_kill_process,
    atom_close, atom_read, atom_write,
    atom_read_string, atom_write_string,
    atom_string_codes, atom_atom_string,
    atom_eof;

bool debug_eval = false;
bool debug_hashtable = false;
bool debug_gc = false;
bool debug_atom = false;
bool debug_parse = false;
bool* debug_enabled = &prelude_loaded;
bool always = true;
bool evaluating = false;

#define guarantee(p, ...) do{ if(!(p)){ fatal_error(__VA_ARGS__); } }while(0)
#define guarantee_errno(p, f) guarantee(p, "%s failed: %s", f, strerror(errno))
#define debug(...) do{ int _debug_res = fprintf(stderr, __VA_ARGS__); guarantee_errno(_debug_res, "fprintf"); }while(0)

void* system_alloc(size_t size){
    void* ret = malloc(size);
    if(!ret){
        fprintf(stderr, "fatal error: memory allocation failed: %s\n", strerror(errno));
        exit(1);
    }
    return ret;
}

void* system_realloc(void* p, size_t size){
    void* ret = realloc(p, size);
    if(!ret){
        fprintf(stderr, "fatal error: memory allocation failed: %s\n", strerror(errno));
        exit(1);
    }
    return ret;
}

void fatal_error(char* format, ...){
    va_list argptr;
    va_start(argptr, format);
    int res = fprintf(stderr, "fatal error: ");
    guarantee_errno(res >= 0, "fprintf");
    res = vfprintf(stderr, format, argptr);
    guarantee_errno(res >= 0, "vfprintf");
    res = fprintf(stderr, "\n");
    guarantee_errno(res >= 0, "fprintf");
    va_end(argptr);
    exit(1);
    UNREACHABLE;
}

void Streams_init(){
    free_stream = 0;
    for(int n = 0; n < MAX_STREAMS; n++){
        streams[n].fd = n + 1;
        streams[n].pos = 1;
        streams[n].alloc_size = 0;
    }
}

void Streams_close_all(){
    while(free_stream < MAX_STREAMS){
        Stream* stream = &streams[free_stream];
        free_stream = stream->fd;
        stream->fd = -1;
    }
    for(int n = 0; n < MAX_STREAMS; n++){
        if(streams[n].fd > 2){
            int res = close(streams[n].fd);
            if(res < 0){
                debug("warning: close failed: %s\n", strerror(errno));
            }
        }
    }
}

int Stream_new(int fd){
    if(free_stream >= MAX_STREAMS){
        fatal_error("too many open streams");
    }
    int ret = free_stream;
    Stream* s = &streams[ret];
    free_stream = s->fd;
    s->fd = fd;
    s->buf = NULL;
    s->pos = 0;
    s->size = 0;
    s->alloc_size = 0;
    return ret;
}

void Stream_close(int n){
    if(n >= MAX_STREAMS || streams[n].pos > streams[n].alloc_size) {
        fatal_error("invalid stream");
    }
    int res = close(streams[n].fd);
    if(res < 0){
        debug("warning: close failed: %s\n", strerror(errno));
    }
    streams[n].fd = free_stream;
    free_stream = n;
    free(streams[n].buf);
    streams[n].pos = 1;
    streams[n].alloc_size = 0;
}

Buffer* Buffer_new(size_t size){
    Buffer* buffer = system_alloc(sizeof(Buffer));
    buffer->size = size;
    buffer->pos = 0;
    buffer->str = system_alloc(size + 1);
    buffer->str[0] = 0;
    buffer->str[size] = 0;
    return buffer;
}

void Buffer_free(Buffer* buffer){
    free(buffer->str);
    free(buffer);
}

void Buffer_resize(Buffer* buffer, size_t size){
    buffer->str = system_realloc(buffer->str, size + 1);
    buffer->size = size;
    buffer->str[size] = 0;
}

HashTable* HashTable_new(size_t size){
    HashTable* table = system_alloc(sizeof(HashTable) + sizeof(Term*) * (size - 1));
    D_HASHTABLE{
        debug("new hashtable %p of size %zu\n", (void*)table, size);
    }
    table->size = size;
    memset(table->table, 0, sizeof(Term*) * size);
    return table;
}

void HashTable_free(HashTable* table){
    D_HASHTABLE{
        debug("freeing hashtable %p\n", (void*)table);
    }
    free(table);
}

bool error(char* format, ...){
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

Pool* Pool_new(){
    Pool* pool = system_alloc(sizeof(Pool));
    pool->sections = 1;
    pool->free = 0;
    pool->terms = system_alloc(sizeof(Term*));
    pool->terms[0] = system_alloc(sizeof(Term) * POOL_SECTION_SIZE);
    return pool;
}

void trace_pool_info(char* str, Pool* pool){
    debug("%s: %zu used, %zu available in %zu sections%s\n",
          str,  pool->free, pool->sections * POOL_SECTION_SIZE,
          pool->sections, would_gc ? " (would gc)" :"");
}

void Term_destroy(Term* term){
    switch(term->type){
    case MOVED:
        return;
    case VAR:
        break;
    case FUNCTOR:
        free(term->data.functor.args);
        break;
    case INTEGER:
        break;
    case STRING:
        free(term->data.string.ptr);
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
    pool->terms = system_realloc(pool->terms, sizeof(Term*) * pool->sections);
    pool->terms[pool->sections-1] = system_alloc(sizeof(Term) * POOL_SECTION_SIZE);
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
        *term = (*term)->data.moved_to;
    }else{
        Term* new = Pool_add_term_expand(pool);
        memcpy(new, *term, sizeof(Term));
        (*term)->type = MOVED;
        (*term)->data.moved_to = new;
        *term = new;
        switch(new->type){
        case VAR:
            Pool_pour(&new->data.var.ref, pool);
            break;
        case FUNCTOR:
            for(functor_size_t i = 0; i < new->data.functor.size; i++){
                Pool_pour(&new->data.functor.args[i], pool);
            }
            break;
        case INTEGER:
        case STRING:
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
    if(keep){
        Pool_pour(&keep, new);
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

string_t mkstring_nt(char* str){
    string_t ret;
    ret.size = strlen(str);
    ret.ptr = memcpy(system_alloc(ret.size + 1), str, ret.size + 1);
    return ret;
}

string_t mkstring(char* str, size_t size){
    string_t ret;
    ret.size = size;
    ret.ptr = memcpy(system_alloc(size + 1), str, size + 1);
    ret.ptr[size] = 0;
    return ret;
}

int stringcmp(string_t a, string_t b){
    if(a.size < b.size){
        return -(int)(b.size - a.size);
    }else if(a.size > b.size){
        return a.size - b.size;
    }else{
        for(int i = 0; i < a.size; i++){
            if(a.ptr[i] != b.ptr[i]){
                return (int)a.ptr[i] - b.ptr[i];
            }
        }
    }
    return 0;
}

Term* String(char* ptr, size_t size){
    Term* term = Pool_add_term_gc(pool);
    term->type = STRING;
    term->data.string = mkstring(ptr, size);
    return term;
}

Term* String_nt(char* str){
    Term* term = Pool_add_term_gc(pool);
    term->type = STRING;
    term->data.string = mkstring_nt(str);
    return term;
}

Term* Atom(atom_t atom){
    if(!atom){
        fatal_error("internal error: atom is null");
    }
    Term* term = Pool_add_term_gc(pool);
    term->type = FUNCTOR;
    term->data.functor.atom = atom;
    term->data.functor.size = 0;
    term->data.functor.args = NULL;
    return term;
}

Term* Functor_unsafe(atom_t atom, functor_size_t size){
    Term* term = Pool_add_term_gc(pool);
    term->type = FUNCTOR;
    term->data.functor.atom = atom;
    term->data.functor.size = size;
    term->data.functor.args = system_alloc(sizeof(Term*) * size);
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

Term* Functor1(atom_t atom, Term* a){
    Term* term = Functor_unsafe(atom, 1);
    Functor_set_arg(term, 0, a);
    return term;
}

Term* Functor2(atom_t atom, Term* a, Term* b){
    Term* term = Functor_unsafe(atom, 2);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    return term;
}

Term* Functor3(atom_t atom, Term* a, Term* b, Term* c){
    Term* term = Functor_unsafe(atom, 3);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    return term;
}

Term* Functor4(atom_t atom, Term* a, Term* b, Term* c, Term* d){
    Term* term = Functor_unsafe(atom, 4);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    return term;
}

Term* Functor5(atom_t atom, Term* a, Term* b, Term* c, Term* d, Term* e){
    Term* term = Functor_unsafe(atom, 5);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    Functor_set_arg(term, 4, e);
    return term;
}

Term* Var(atom_t name){
    Term* term = Pool_add_term_gc(pool);
    term->type = VAR;
    term->data.var.name = name;
    term->data.var.ref = term;
    return term;
}

hash_t hash_byte(uint8_t c, hash_t hash){
    return (hash ^ c) * HASH_PRIME;
}

hash_t hash_string(string_t str, hash_t hash){
    for(size_t i = 0; i < str.size; i++){
        hash = hash_byte(str.ptr[i], hash);
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

hash_t hash_atom(atom_t atom, hash_t hash){
    char* c = (char*)&atom;
    for(int i = 0; i < sizeof(atom); i++){
        hash = hash_byte(c[i], hash);
    }
    return hash;
}

hash_t hash_rec(Term* term, hash_t hash){
    switch(term->type){
    case INTEGER:
        return hash_integer(term->data.integer, hash);
    case FUNCTOR:
        hash = hash_atom(term->data.functor.atom, hash);
        functor_size_t size = term->data.functor.size;
        if(size){
            hash = hash_byte(size, hash);
            for(functor_size_t i = 0; i < size; i++){
                hash = hash_rec(term->data.functor.args[i], hash);
            }
        }
        return hash;
    case STRING:
        return hash_string(term->data.string, hash);
    case VAR:
        fatal_error("Cannot hash variable '%s'", term->data.var.name);
    case MOVED:
        fatal_error("Cannot hash a moved term");
    }
    UNREACHABLE;
}

hash_t hash(Term* term){
    uint32_t hash = HASH_INIT;
    return hash_rec(term, hash);
}

atom_t intern(char* string){
    disable_gc();
    Term* str = String_nt(string);
    Term** term = HashTable_get(interned, str);
    if(*term){
        if((*term)->type != FUNCTOR){
            fatal_error("interned term is not an atom");
        }
        D_ATOM{
            debug("already interned %s as %lu\n", string, (*term)->data.functor.atom);
        }
        enable_gc();
        return (*term)->data.functor.atom;
    }
    atom_t atom = next_free_atom++;
    *term = Atom(atom);
    Term** rev = HashTable_get(atom_names, *term);
    *rev = str;
    D_ATOM{
        debug("interning %s as %lu\n", string, atom);
    }
    enable_gc();
    return atom;
}

void intern_prim(char* string, atom_t atom){
    disable_gc();
    Term* str = String_nt(string);
    Term** term = HashTable_get(interned, str);
    if(*term){
        fatal_error("prim '%s' already exists", string);
    }
    *term = Atom(atom);
    Term** rev = HashTable_get(atom_names, *term);
    *rev = str;
    D_ATOM{
        debug("interning primitve %s as %lu\n", string, atom);
    }
    enable_gc();
}

Term* Spec(atom_t atom, int size){
    disable_gc();
    return Functor2(atom_slash, Atom(atom), Integer(size));
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
    while(term->type == VAR && term->data.var.ref != term){
        term = term->data.var.ref;
    }
    return term;
}

string_t atom_string(atom_t atom){
    static char buf[32];
    Term* term = HashTable_find(atom_names, Atom(atom));
    if(!term){
        int n = snprintf(buf, sizeof(buf), "#atom%lu", atom);
        return String(buf, n)->data.string;
    }
    if(term->type != STRING) fatal_error("atom names table contains non-string");
    return term->data.string;
}

void Term_render(Term* term, bool show_vars, renderer_t write, void* data){
    if(!term){
        write(data, "NULL", 4);
        return;
    }
    if(!show_vars){
        term = chase(term);
    }
    switch(term->type){
    case MOVED:
        write(data, "_MOVED", 6);
        break;
    case VAR:
        if(term->data.var.ref != term){
            Term_render(term->data.var.ref, show_vars, write, data);
        }else{
            string_t name = atom_string(term->data.var.name);
            write(data, name.ptr, name.size);
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
        write(data, term->data.string.ptr, term->data.string.size);
        write(data, "\"", 1);
        break;
    case FUNCTOR: {
        string_t name = atom_string(term->data.functor.atom);
        write(data, name.ptr, name.size);
        if(term->data.functor.size){
            write(data, "(", 1);
            for(int i = 0; i < term->data.functor.size; i++){
                Term_render(term->data.functor.args[i], show_vars, write, data);
                if(i + 1 < term->data.functor.size){
                    write(data, ", ", 2);
                }
            }
            write(data, ")", 1);
        }
    } break;
    }
}

Buffer* Term_show(Term* term, bool show_vars){
    Buffer* buffer = Buffer_new(DEFAULT_BUFFER_SIZE);
    Term_render(term, show_vars, (renderer_t)Buffer_append, buffer);
    Buffer_resize(buffer, buffer->pos + 1);
    return buffer;
}

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

void trace_term(char* format, Term* term, ...){
    char buf[80];
    va_list argptr;
    va_start(argptr, term);
    vfprintf(stderr, format, argptr);
    Buffer* buffer = Term_show(term, true);
    debug(": %s\n", short_snippet(buffer->str, buf, sizeof buf));
    Buffer_free(buffer);
    va_end(argptr);
}

Term** Functor_get(Term* term, atom_t atom, functor_size_t size){
    if(term->type != FUNCTOR ||
       term->data.functor.atom != atom ||
       term->data.functor.size != size){
        return NULL;
    }
    return term->data.functor.args;
}

bool is_Atom(Term* term){
    return term->type == FUNCTOR && term->data.functor.size == 0;
}

bool Atom_eq(Term* term, atom_t atom){
    if(term->type != FUNCTOR ||
       term->data.functor.atom != atom ||
       term->data.functor.size != 0){
        return false;
    }
    return true;
}

Term* List_head(Term* list){
    Term** args = Functor_get(list, atom_cons, 2);
    if(!args){
        trace_term("list", list);
        fatal_error("head: expected non-empty list");
    }
    return args[0];
}

Term* List_tail(Term* list){
    Term** args = Functor_get(list, atom_cons, 2);
    if(!args){
        trace_term("list", list);
        fatal_error("tail: expected non-empty list");
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
    case STRING:
        return !stringcmp(a->data.string, b->data.string);
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
    UNREACHABLE;
}

Term** Assoc_get(Term** assoc, Term* key){
    for(Term* list = *assoc; !Atom_eq(list, atom_nil); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), atom_colon, 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            D_HASHTABLE{
                if(Atom_eq(args[1], atom_nil)){
                    trace_term("hash collision", key);
                }
            }
            return &args[1];
        }
    }
    disable_gc();
    Term* pair = Functor2(atom_colon, key, NULL);
    *assoc = Functor2(atom_cons, pair, *assoc);
    enable_gc();
    return &pair->data.functor.args[1];
}

Term* Assoc_find(Term* assoc, Term* key){
    for(Term* list = assoc; !Atom_eq(list, atom_nil); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), atom_colon, 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            D_HASHTABLE{
                if(Atom_eq(args[1], atom_nil)){
                    trace_term("hash collision", key);
                }
            }
            return args[1];
        }
    }
    return NULL;
}

Term** HashTable_get(HashTable* table, Term* key){
    hash_t hkey = hash(key);
    Term** assoc = &table->table[hkey % table->size];
    D_HASHTABLE{
        debug("hashtable %p get:\n", (void*)table);
        trace_term("key (hash %u)", key, hkey);
        trace_term("assoc", *assoc);
    }
    disable_gc();
    if(!*assoc){
        *assoc = Atom(atom_nil);
    }
    Term** val = Assoc_get(assoc, key);
    enable_gc();
    D_HASHTABLE{
        trace_term("val", *val);
    }
    return val;
}

void HashTable_append(HashTable* table, Term* key, Term* val){
    Term** list = HashTable_get(table, key);
    disable_gc();
    (*list) = Functor2(atom_cons, val, *list ? *list : Atom(atom_nil));
    enable_gc();
    D_HASHTABLE{
        debug("hashtable %p: append\n", (void*)table);
        trace_term("key", key);
        trace_term("val", val);
    }
}

Term* HashTable_find(HashTable* table, Term* key){
    Term *assoc = table->table[hash(key) % table->size];
    if(!assoc){
        return NULL;
    }
    return Assoc_find(assoc, key);
}

void render_fprintf(FILE* out, char* str, size_t size){
    int res = fprintf(out, "%.*s", (int)size, str);
    guarantee_errno(res >= 0, "fprintf");
}

void Term_print(Term* term){
    Term_render(term, false, (renderer_t)render_fprintf, stdout);
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

bool prim_op(Term** args){
    HashTable_append(ops, args[2], Functor3(atom_op, args[0], args[1], args[2]));
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
            *copy = Var(term->data.var.name);
        }
        return *copy;
    }
    case MOVED:
        fatal_error("Cannot copy a moved term");
        return NULL;
    default:
        UNREACHABLE;
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

bool Rule_spec(Term* term, atom_t* atom, int* size){
    Term** args = Functor_get(term, atom_entails, 2);
    if(args){
        term = args[0];
    }
    if(term->type == FUNCTOR){
        (*atom) = term->data.functor.atom;
        (*size) = term->data.functor.size;
        return true;
    }
    return false;
}

void add_undo_var(Term* var){
    Term** args = Functor_get(stack, atom_frame, 3);
    if(!args) return;
    Term** undo_vars = &args[1];
    if(!Atom_eq(*undo_vars, atom_drop)){
        *undo_vars = Functor2(atom_cons, var, *undo_vars);
    }
}

void add_undo_vars(Term* vars){
    if(Atom_eq(vars, atom_drop)) return;
    Term** args = Functor_get(stack, atom_frame, 3);
    if(!args) return;
    Term** undo_vars = &args[1];
    if(Atom_eq(*undo_vars, atom_drop)) return;
    for(; !Atom_eq(vars, atom_nil); vars = List_tail(vars)){
        Term* var = List_head(vars);
        *undo_vars = Functor2(atom_cons, var, *undo_vars);
    }
}

void reset_undo_vars(Term* vars){
    if(Atom_eq(vars, atom_drop)) return;
    for(; !Atom_eq(vars, atom_nil); vars = List_tail(vars)){
        Term* var = List_head(vars);
        if(var->type != VAR) fatal_error("cannot reset non-var");
        var->data.var.ref = var;
    }
}

bool stack_push(atom_t atom, functor_size_t size, Term* term){
    disable_gc();
    Term* rules = HashTable_find(globals, Spec(atom, size));
    enable_gc();
    if(!rules){
        return error("No such predicate '%s'/%u", atom_string(atom).ptr, size);
    }
    disable_gc();
    Term* branches = Atom(atom_nil);
    for(; !Atom_eq(rules, atom_nil); rules = List_tail(rules)){
        Term* head = Term_copy(List_head(rules));
        Term* branch;
        Term** args = Functor_get(head, atom_entails, 2);
        if(args){
            branch = Functor2(atom_comma, Functor2(atom_eq, term, args[0]), args[1]);
        }else{
            branch = Functor2(atom_eq, term, head);
        }
        if(next_query){
            branch = Functor2(atom_comma, branch, next_query);
        }
        branches = Functor2(atom_cons, branch, branches);
    }
    if(Atom_eq(branches, atom_nil)){
        enable_gc();
        return error("No rules for predicate '%s/%u'", atom_string(atom), size);
    }
    stack = Functor3(atom_frame, branches, Atom(atom_nil), stack);
    next_query = NULL;
    enable_gc();
    return true;
}

bool stack_next(bool success){
    D_EVAL{
        debug("stack_next(%s)\n", success ? "true" : "fail");
        trace_term("stack_next stack", stack);
        if(next_query){
            trace_term("stack_next next_query", next_query);
        }
    }
    if(Atom_eq(stack, atom_empty)){
        return false;
    }
    Term** args = Functor_get(stack, atom_frame, 3);
    if(!args){
        fatal_error("stack should be empty/0 or frame/3");
    }
    Term** branches = &args[0];
    Term** vars = &args[1];
    Term* parent = args[2];
    if(success){
        disable_gc();
        stack = parent;
        add_undo_vars(*vars);
        enable_gc();
        next_query = Atom(atom_true);
        return true;
    }else{
        reset_undo_vars(*vars);
        *vars = Atom(atom_nil);
        Term** car_cdr = Functor_get(*branches, atom_cons, 2);
        if(car_cdr){
            Term** cadr_args = Functor_get(car_cdr[1], atom_frame, 3);
            if(Atom_eq(car_cdr[1], atom_nil) ||
               (cadr_args && Atom_eq(cadr_args[1], atom_drop))){
                *vars = Atom(atom_drop);
            }
            *branches = car_cdr[1];
            next_query = car_cdr[0];
            return true;
        }else{
            stack = parent;
            return stack_next(false);
        }
    }
}

void set_var(Term* a, Term* b){
    D_EVAL{
        trace_term("unifying %s with", b, atom_string(a->data.var.name));
    }
    if(a->type != VAR) fatal_error("Called set_var on non-var");
    if(a->data.var.ref != a) fatal_error("Cannot overwrite variable's value");
    a->data.var.ref = b;
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
    case STRING:
        return !stringcmp(a->data.string, b->data.string);
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
    case MOVED:
        fatal_error("Cannot unify a moved term");
    case VAR:
        UNREACHABLE;
    }
    UNREACHABLE;
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
        }
    }
    default:
        fatal_error("invalid math expression");
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

bool prim_cut(Term** args){
    disable_gc();
    Term** frame = Functor_get(stack, atom_frame, 3);
    frame[0] = Atom(atom_nil);
    enable_gc();
    return true;
}

bool prim_assertz(Term** args){
    return assertz(args[0]);
}

bool prim_is(Term** args) {
    Term* lhs = chase(args[0]);
    Term* rhs = chase(args[1]);
    disable_gc();
    bool ret = unify(lhs, Integer(eval_math(rhs)));
    enable_gc();
    return ret;
}

bool prim_univ(Term** args){
    Term* functor = chase(args[0]);
    Term* repr = chase(args[1]);
    if(functor->type == VAR){
        Term* name = chase(List_head(repr));
        if(!is_Atom(name)) fatal_error("atom expected in '=..'/2");
        functor_size_t size = 0;
        for(Term* list = chase(List_tail(repr));
            !Atom_eq(list, atom_nil);
            list = chase(List_tail(list))){
            size++;
        }
        disable_gc();
        Term* term = Functor_unsafe(name->data.functor.atom, size);
        functor_size_t i = 0;
        for(Term* list = chase(List_tail(repr));
            !Atom_eq(list, atom_nil);
            list = chase(List_tail(list)), i++){
            Functor_set_arg(term, i, chase(List_head(list)));
        }
        bool ret = unify(functor, term);
        enable_gc();
        return ret;
    }else if(functor->type == FUNCTOR){
        disable_gc();
        Term* list = Functor2(atom_cons, Atom(functor->data.functor.atom), NULL);
        Term** rest = &list->data.functor.args[1];
        for(functor_size_t i = 0; i < functor->data.functor.size; i++){
            *rest = Functor2(atom_cons, functor->data.functor.args[i], NULL);
            rest = &(*rest)->data.functor.args[1];
        }
        *rest = Atom(atom_nil);
        bool ret = unify(repr, list);
        enable_gc();
        return ret;
    }else{
        fatal_error("invalid arguments to '=..'/2");
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
        Streams_close_all();
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

string_t Term_string(Term* term){
    term = chase(term);
    if(term->type == STRING){
        return term->data.string;
    }else if(term->type == FUNCTOR && term->data.functor.size == 0){
        return atom_string(term->data.functor.atom);
    }else{
        fatal_error("expected string");
        UNREACHABLE;
    }
}

integer_t Term_integer(Term* term){
    term = chase(term);
    if(term->type != INTEGER){
        fatal_error("expected integer");
    }
    return term->data.integer;
}

bool prim_process_create(Term** args){
    disable_gc();
    char* command_path = Term_string(args[0]).ptr;
    char* command_args[256];
    command_args[0] = command_path;
    int n = 1;
    for(Term* list = args[1]; !Atom_eq(list, atom_nil); list = List_tail(list)){
        if(n >= sizeof(command_args) - 1){
            fatal_error("too many arguments for process");
        }
        command_args[n] = Term_string(List_head(list)).ptr;
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
    int stream = Term_integer(args[0]);
    string_t str = Term_string(args[1]);
    ssize_t res = write(streams[stream].fd, str.ptr, str.size);
    if(res >= 0){
        return true;
    }else{
        return error("warning: write failed: %s\n", strerror(errno));
    }
}

bool prim_read_string(Term** args){
    // TODO: use buffer
    integer_t stream = Term_integer(args[0]);
    integer_t max = Term_integer(args[1]);
    char buf[max];
    ssize_t res = read(streams[stream].fd, buf, max);
    if(res == 0){
        return unify(args[2], Atom(atom_eof));
    }else if(res >= 1){
        return unify(args[2], String(buf, res));
    }else{
        return error("warning: read failed: %s\n", strerror(errno));
    }
}

prim_t find_prim(atom_t atom, functor_size_t size){

#define PRIM(f, n, r) \
    static atom_t atom_ ## r = 0; \
    if(!atom_ ## r) atom_ ## r = intern(#f); \
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
    //PRIM(write, 2, write);
    //PRIM(read, 2, read);
    PRIM(write_string, 2, write_string);
    PRIM(read_string, 3, read_string);
    //PRIM(string_codes, 2, string_codes);
    //PRIM(atom_string, 2, atom_string);
#undef PRIM

    return NULL;
}

bool eval_query(){
    if(evaluating){
        fatal_error("internal error: called eval_query recursively");
    }
    evaluating = true;
    while(true){
        bool success = true;
        Term* term = chase(query);
        switch(term->type){
        case INTEGER:
            evaluating = false;
            return error("Cannot eval integer %ld", term->data.integer);
        case STRING:
            evaluating = false;
            return error("Cannot eval string \"%s\"", term->data.string);
        case VAR:
            evaluating = false;
            return error("Cannot eval unbound variable '%s'", atom_string(term->data.var.name));
        case MOVED:
            fatal_error("Cannot eval moved term");
        case FUNCTOR: {
            atom_t atom = term->data.functor.atom;
            functor_size_t size = term->data.functor.size;
            Term** args = term->data.functor.args;
            if(atom == atom_comma && size == 2){
                disable_gc();
                next_query = next_query ? Functor2(atom_comma, args[1], next_query) : args[1];
                query = args[0];
                enable_gc();
                continue;
            }
            D_EVAL{
                trace_term("eval term", term);
            }
            prim_t prim = find_prim(atom, size);
            if(prim){
                success = prim(args);
            }else{
                if(!stack_push(atom, size, term)){
                    evaluating = false;
                    return false;
                }
                success = false;
            }
            if(!success || !next_query){
                if(!stack_next(success)){
                    D_EVAL{ trace_term("eval stack", stack); }
                    evaluating = false;
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
    return !isalnum(c) && !isspace(c) && !strchr("()[],'_", c) && isprint(c);
}

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

Term* parse_args(char **str, atom_t atom, HashTable* vars){
    char* pos = spaces(*str);
    if(*pos != '('){
        return NULL;
    }
    pos++;
    Term* list = Atom(atom_nil);
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
                term = Var(atom_underscore);
            }else{
                Term** var_term = HashTable_get(vars, String_nt(buf));
                if(!*var_term){
                    *var_term = Var(intern(buf));
                }
                term = *var_term;
            }
        }else{
            term = Atom(intern(buf));
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
                return Atom(atom_nil);
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
            *rest = Atom(atom_nil);
            return list;
        }else{
            return NULL;
        }
    }
}

Term* parse_simple_term(char** str, HashTable* vars){
    char* pos = spaces(*str);
    switch(*pos){
    case '(':
        return parse_parens(str, vars);
    case '[':
        return parse_list(str, vars);
    case '.': {
        char next = *(pos + 1);
        if(isspace(next) || !next) return NULL; }
    }
    Term* atom = parse_atomic(&pos, vars);
    if(!atom) return NULL;
    if(is_Atom(atom) && HashTable_find(ops, atom)){
        *str = pos;
        return atom;
    }
    if(atom->type == FUNCTOR && atom->data.functor.size == 0){
        Term* functor = parse_args(&pos, atom->data.functor.atom, vars);
        if(functor){
            *str = pos;
            return functor;
        }
    }
    *str = pos;
    return atom;
}

void op_type(integer_t prec, atom_t atom, integer_t *left, integer_t *right){
    char* spec = atom_string(atom).ptr;
    char* r = spec + 2;
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
    Term* left_term = NULL;
    while(true){
        if(!*pos){
            D_PARSE{ debug("no more terms to combine\n"); }
            *terms = pos;
            return left_term;
        }
        Term* ret = NULL;
        Term** ret_pos = NULL;
        Term* list = is_Atom(*pos) ? HashTable_find(ops, *pos) : NULL;
        if(!list){
            if(!left_term){
                left_term = *pos++;
                if(!*pos){
                    *terms = pos;
                    return left_term;
                }
                list = is_Atom(*pos) ? HashTable_find(ops, *pos) : NULL;
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
        for(; !Atom_eq(list, atom_nil); list = List_tail(list)){
            Term* op = List_head(list);
            D_PARSE{ trace_term("trying op", op); }
            Term** args = Functor_get(op, atom_op, 3);
            if(!args || !args[0]->type == INTEGER || args[1]->type != FUNCTOR){
                fatal_error("invalid entry in ops table");
            }
            integer_t left_prec, right_prec;
            atom_t op_spec = args[1]->data.functor.atom;
            op_type(args[0]->data.integer, op_spec, &left_prec, &right_prec);
            if(left_prec && !left_term){ 
                D_PARSE{
                    debug("missing left operand for '%s' '%s'\n",
                            atom_string(op_spec).ptr, atom_string(name).ptr);
                }
                continue;
            }
            if(left_term && !left_prec){
                D_PARSE{
                    debug("extra left operand for '%s' '%s'\n",
                            atom_string(op_spec).ptr, atom_string(name).ptr);
                }
                continue;
            }
            if(left_prec > prec){
                D_PARSE{
                    debug("dropping '%s', too loose (%ld <= %ld)\n",
                            atom_string(name).ptr, left_prec, prec);
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

Term* parse_term_partial(char** str){
    HashTable* vars = HashTable_new(PARSE_VARS_HASHTABLE_SIZE);
    disable_gc();
    Term* term = parse_term_vars(str, vars, NULL);
    enable_gc();
    HashTable_free(vars);
    return term;
}

Term* parse_term(char* str){
    D_PARSE{ debug("\nparsing str: %s\n", str); }
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
    Term* list = NULL;
    Term** rest = &list;
    for(; *pos; pos = spaces(pos)){
        Term* term = parse_term_partial(&pos);
        if(!term){
            return NULL;
        }
        pos = spaces(pos);
        if(*pos != '.'){
            return NULL;
        }
        pos++;
        *rest = Functor2(atom_cons, term, NULL);
        rest = &(*rest)->data.functor.args[1];
    }
    *rest = Atom(atom_nil);
    enable_gc();
    return list;
}

bool assertz(Term* term){
    Term** args = Functor_get(term, atom_entails, 2);
    if(args){
        Term* head = chase(args[0]);
        if(head->type != FUNCTOR){
            D_EVAL{
                trace_term("assertz: not a functor", head);
            }
            return false;
        }
        disable_gc();
        HashTable_append(globals, Spec(head->data.functor.atom, head->data.functor.size), term);
        enable_gc();
        return true;
    }
    args = Functor_get(term, atom_rarrow, 2);
    if(args){
        Term* q = Functor1(atom_assertz_dcg, term);
        if(evaluating){
            next_query = next_query ? Functor2(atom_cons, q, next_query) : q;
        }else{
            query = q;
            return eval_query();
        }
    }
    disable_gc();
    HashTable_append(globals, Spec(term->data.functor.atom, term->data.functor.size), term);
    enable_gc();
    return true;
}

void eval_toplevel(Term* term){
    if(term->type != FUNCTOR){
        trace_term("eval_toplevel term", term);
        fatal_error("toplevel term must be functor");
    }
    Term** args = Functor_get(term, atom_entails, 1);
    if(args){
        query = args[0];
        if(!eval_query()){
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

Term* parse_file(char* path){
    FILE* fp = fopen(path, "r");
    if(!fp){
        fatal_error("could not open '%s': %s", path, strerror(errno));
    }
    int res = fseek(fp, 0, SEEK_END); guarantee_errno(res >= 0, "fseek");
    long size = ftell(fp); guarantee_errno(size >= 0, "ftell");
    res = fseek(fp, 0, SEEK_SET); guarantee_errno(res >= 0, "fseek");

    Buffer* data = Buffer_new(size);
    size_t res_size = fread(data->str, size, 1, fp); guarantee(res_size == 1, "fread failed");
    res = fclose(fp); guarantee(res >= 0, "fclose");

    Term* list = parse_toplevel(data->str);

    Buffer_free(data);

    return list;
}

void load_file(char* path){
    keep = parse_file(path);
    if(!keep){
        fatal_error("failed to parse file '%s'", path);
    }
    for(; !Atom_eq(keep, atom_nil); keep = List_tail(keep)){
        eval_toplevel(List_head(keep));
    }
    keep = NULL;
}

void load_prelude(){
    disable_gc();

#define ADD_OP(prec, order, name) \
    HashTable_append(ops, Atom(intern(name)), Functor3(atom_op, Integer(prec), Atom(intern(order)), Atom(intern(name))))
    ADD_OP(1200, "xfx", ":-");
    ADD_OP(1200, "xfx", "-->");
    ADD_OP(1200, "fx", ":-");
    ADD_OP(1100, "xfy", ";");
    ADD_OP(1000, "xfy", ",");
    ADD_OP(700, "xfx", "=");
    ADD_OP(700, "xfx", "=..");
    ADD_OP(500, "yfx", "+");
    ADD_OP(700, "xfx", "is");
#undef ADD_OP

    enable_gc();

    load_file(PRELUDE_PATH);

    prelude_loaded = true;
}

void list_vars(Term* term, HashTable* vars){
    switch(term->type){
    case VAR:
        if(atom_string(term->data.var.name).ptr[0] == '_') return;
        Term** val = HashTable_get(vars, Integer((integer_t)term));
        if(!*val) *val = term;
        break;
    case FUNCTOR:
        for(functor_size_t i = 0; i < term->data.functor.size; i++){
            list_vars(term->data.functor.args[i], vars);
        }
        break;
    default:
        (void)0;
    }
}

Term* vars_of(Term* term){
    HashTable* vars = HashTable_new(PARSE_VARS_HASHTABLE_SIZE);
    disable_gc();
    list_vars(term, vars);
    Term *list = Atom(atom_nil);
    for(size_t i = 0; i < vars->size; i++){
        Term* assoc = vars->table[i];
        if(assoc){
            for(; !Atom_eq(assoc, atom_nil); assoc = List_tail(assoc)){
                Term** args = Functor_get(List_head(assoc), atom_colon, 2);
                Term* var = Var(((Term*)args[0]->data.integer)->data.var.name);
                list = Functor2(atom_cons, Functor2(atom_eq, var, args[1]), list);
            }
        }
    }
    enable_gc();
    return list;
}

void eval_interactive(Term* term){
    keep = vars_of(term);
    query = term;
    if(eval_query()){
        if(Atom_eq(keep, atom_nil)){
            printf("yep.\n");
        }else{
            for(; !Atom_eq(keep, atom_nil); keep = List_tail(keep)){
                Buffer* buffer = Term_show(List_head(keep), false);
                printf("%s.\n", buffer->str);
                Buffer_free(buffer);
            }
        }
    }else{
        printf("nope.\n");
    }
}

void eval_stdin(char* prompt, void (*eval)(Term*)){
    Buffer* buffer = Buffer_new(4096);
    bool term = isatty(0);
    if(term){
        printf("%s", prompt);
        fflush(stdout);
    }
    while(true){
        if(buffer->size == buffer->pos){
            Buffer_resize(buffer, buffer->size * 2);
        }
        ssize_t n = read(0, buffer->str + buffer->pos, buffer->size - buffer->pos);
        if(n < 0){
            fatal_error("read error: %s", strerror(errno));
        }
        if(!n){
            if(buffer->pos){
                fatal_error("could not parse: %s", buffer->str);
            }
            if(term){
                printf("\n");
            }
            return;
        }
        buffer->pos += n;
        buffer->str[buffer->pos] = 0;

        char* pos = buffer->str;
        char* next = NULL;
        while(true){
            Term* term = parse_term_partial(&pos);
            if(!term){
                break;
            }
            pos = spaces(pos);
            if(*pos != '.'){
                break;
            }
            pos++;
            pos = spaces(pos);
            next = pos;
            eval(term);
            if(term){
                printf("%s", prompt);
                fflush(stdout);
            }
        }
        if(next){
            size_t remaining = buffer->pos - (next - buffer->str);
            memmove(buffer->str, next, remaining + 1);
            buffer->pos = remaining;
        }
    }
}

int main(int argc, char** argv){
    char** args = argv + 1;
    char* arg;
    char* file = NULL;
    char* eval = NULL;
    char usage[] = "usage: poorlog [FILE] [-e EXPR] [-dparse] [-deval] [-dhashtable] [-dgc] [-datom] [-dprelude]";
    while(*args){
        arg = *args++;
        if(*arg != '-' || (arg[0] && arg[1] == 0)){
            if(!file){
                file = arg;
            }else{
                fatal_error("too many files on command line: %s", arg);
            }
            continue;
        }
        switch(arg[1]){
        case 'e':
            eval = *args++;
            if(!eval) fatal_error("'-e' requires and argument");
            break;
        case 'h':
            printf("%s\n", usage);
            exit(0);
            break;
        case 'd': 
            if(!strcmp(arg+2, "parse")) debug_parse = true; else
            if(!strcmp(arg+2, "eval")) debug_eval = true; else
            if(!strcmp(arg+2, "hashtable")) debug_hashtable = true; else
            if(!strcmp(arg+2, "gc")) debug_gc = true; else
            if(!strcmp(arg+2, "atom")) debug_atom = true; else
            if(!strcmp(arg+2, "prelude")) debug_enabled = &always;
            else fatal_error("unknown debug mode: %s", arg+2);
            break;
        default:
            fatal_error("unknown argument: %s\n%s\n", arg, usage);
            exit(1);
        }
    }

    pool = Pool_new();
    globals = HashTable_new(GLOBALS_SIZE);
    ops = HashTable_new(OPS_HASHTABLE_SIZE);
    interned = HashTable_new(INTERNED_TABLE_SIZE);
    atom_names = HashTable_new(INTERNED_TABLE_SIZE);
    Streams_init();

    atom_cons = 1;
    atom_nil = 2;
    atom_colon = 3;
    next_free_atom = 4;
    intern_prim(".", atom_cons);
    intern_prim("[]", atom_nil);
    intern_prim(":", atom_colon);

    atom_slash = intern("/");
    atom_op = intern("op");
    atom_entails = intern(":-");
    atom_drop = intern("drop");
    atom_frame = intern("frame");
    atom_eq = intern("=");
    atom_comma = intern(",");
    atom_empty = intern("empty");
    atom_true = intern("true");
    atom_underscore = intern("_");
    atom_rarrow = intern("-->");
    atom_assertz_dcg = intern("assertz_dcg");
    atom_is = intern("is");
    atom_add = intern("+");
    atom_process_create = intern("process_create");
    atom_kill_process = intern("kill_process");
    atom_close = intern("close");
    atom_read = intern("read");
    atom_write = intern("write");
    atom_read_string = intern("read_string");
    atom_write_string = intern("write_string");
    atom_string_codes = intern("string_codes");
    atom_atom_string = intern("atom_string");
    atom_eof = intern("eof");

    stack = Atom(atom_empty);

    load_prelude();

    if(file){
        if(!strcmp(file, "-")){
            eval_stdin("| ", eval_toplevel);
        }else{
            load_file(file);
        }
    }

    if(eval){
        Term* term = parse_term(eval);
        if(!term) fatal_error("Could not parse command-line expression");
        eval_interactive(term);
    }

    if(!eval && !file){
        eval_stdin("?- ", eval_interactive);
    }

    return 0;
}
