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

#define LIB_PATH "boot/lib"

#define GLOBALS_SIZE 4096
#define POOL_SECTION_SIZE 4096
#define INTERNED_TABLE_SIZE 4096
#define COLLISION_HASHTABLE_SIZE 1024
#define PARSE_VARS_HASHTABLE_SIZE 1024
#define OPS_HASHTABLE_SIZE 1024
#define MAX_NO_PAREN_TERMS 1024
#define MAX_STREAMS 256
#define MAX_C_TERMS 1024

#define HASH_INIT 2166136261
#define HASH_PRIME 16777619

#define UNREACHABLE __builtin_unreachable()

#define D_PARSE DEBUG_IF(debug_parse)
#define D_EVAL DEBUG_IF(debug_eval)
#define D_GC DEBUG_IF(debug_gc)
#define D_HASHTABLE DEBUG_IF(debug_hashtable)
#define D_ATOM DEBUG_IF(debug_atom)
#define D_STRING DEBUG_IF(debug_string)
#define D_SANITY DEBUG_IF(debug_sanity)

#ifdef ISABLE_DEBUG
#define DEBUG_IF(_) if(0)
#else
#define DEBUG_IF(p) \
    for(bool _debug_if = (disable_gc(), true); \
        _debug_if && p && *debug_enabled; \
        _debug_if = false, enable_gc())
#endif

#define SANITY_CHECK D_SANITY{ sanity_check_all(); }

#ifdef ISABLE_ASSERTS
#define ASSERT_CODE(...)
#define assert(...)
#else
#define ASSERT_CODE(...) __VA_ARGS__
#define assert(p, ...) do{ if(!(p)){ fatal_error("assert failed `" #p "': " __VA_ARGS__); } }while(0)
#endif

typedef uint32_t hash_t;
typedef uint8_t functor_size_t;
typedef int64_t integer_t;
typedef uint64_t atom_t;

typedef struct {
    size_t alloc_size;
    size_t end;
    char* ptr;
} Buffer;

struct HashTable;

typedef struct Term {
    enum { FUNCTOR, VAR, MOVED, INTEGER, STRING, DICT } type;
    union {
        integer_t integer;
        Buffer string;
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
        struct HashTable* dict;
    } data;
} Term;

typedef void (*renderer_t)(void*, char*, size_t);
typedef void(*term_iterator_t)(Term**, void*);
typedef bool (*prim_t)(Term**);
typedef hash_t (*hash_function_t)(Term*);

typedef struct {
    size_t sections;
    size_t free;
    Term** terms;
} Pool;

typedef struct HashTable {
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

enum render_flags_t {
    RENDER_DEFAULT = 0,
    RENDER_NO_CHASE = 1,
    RENDER_STRICT = 2,
    RENDER_NO_OP = 4
};

#define MIN(a,b) ((a) < (b) ? a : b)
#define MAX(a,b) ((a) < (b) ? b : a)

#define BREAKPOINT raise(SIGTRAP)

void trace_term(char* str, Term* term, ...);
Term* parse_term_vars(char** str, HashTable* vars, char* end_char);
bool assertz(Term* term);
Term* HashTable_get(HashTable* table, Term* key);
Term* HashTable_find(HashTable* table, Term* key);
void fatal_error_(const char* func, const char* file, int line, char* format, ...);
void load_file(char* path);
void sanity_check_all();
void disable_gc();
void enable_gc();
bool is_Atom(Term* term);
bool Atom_eq(Term* term, atom_t atom);
Term** Functor_get(Term* term, atom_t atom, functor_size_t size);
Term* List_head(Term* list);
integer_t Term_integer(Term* term);
Term* List_tail(Term* list);
void Term_render(Term* term, int render_flags, int left_prec, int right_prec, bool in_list, renderer_t write, void* data);
void set_var(Term* a, Term* b);
Term* Var(atom_t name);
Term* chase(Term* term);

Pool* pool = NULL;

struct roots_t {
    HashTable* globals;
    HashTable* ops;
    HashTable* interned;
    HashTable* atom_names;
    Term* nil;
    Term** c_terms[MAX_C_TERMS];
} root;

size_t next_c_term = 0;
size_t c_frame_count = 0;
const char* current_frame_func;

int gc_disable_count = 0;
bool would_gc = false;
bool base_loaded = false;
Stream streams[MAX_STREAMS];
int free_stream = 0;
int def_outer_prec = 1200;

typedef struct eval_env_t {
    Term* query;
    Term* next_query;
    Term* stack;
    struct eval_env_t* prev_eval;
} eval_env_t;

typedef struct trace_info_t {
    int depth;
    Term* string;
} trace_info_t;

eval_env_t* current_eval_env;

#define EACH_BUILTIN_ATOM(F) \
    F(atom_slash, "/") \
    F(atom_colon, ":") \
    F(atom_nil, "[]") \
    F(atom_cons, ".") \
    F(atom_op, "op") \
    F(atom_entails, ":-") \
    F(atom_frame, "frame") \
    F(atom_drop, "drop") \
    F(atom_comma, ",") \
    F(atom_eq, "=") \
    F(atom_empty, "empty") \
    F(atom_true, "true") \
    F(atom_underscore, "_") \
    F(atom_assertz_dcg, "assertz_dcg") \
    F(atom_long_rarrow, "-->") \
    F(atom_braces, "{}") \
    F(atom_library, "library") \
    F(atom_is, "is") \
    F(atom_add, "+") \
    F(atom_mul, "*") \
    F(atom_process_create, "process_create") \
    F(atom_kill_process, "kill_process") \
    F(atom_close, "close") \
    F(atom_read, "read") \
    F(atom_write, "write") \
    F(atom_read_string, "read_string") \
    F(atom_write_string, "write_string") \
    F(atom_string_codes, "string_codes") \
    F(atom_atom_string, "atom_string") \
    F(atom_eof, "eof") \
    F(atom_string_concat, "string_concat") \
    F(atom_string, "string") \
    F(atom_string_first, "string_first") \
    F(atom_var, "var") \
    F(atom_listing, "listing") \
    F(atom_xf, "xf") \
    F(atom_yf, "yf") \
    F(atom_xfx, "xfx") \
    F(atom_xfy, "xfy") \
    F(atom_yfx, "yfx") \
    F(atom_fx, "fx") \
    F(atom_fy, "fy") \
    F(atom_c_land, "c_land")

#define DECLARE_ATOM(a, _) a,
enum builtin_atoms_t {
    atom_invalid = 0,
    EACH_BUILTIN_ATOM(DECLARE_ATOM)
    first_free_atom
};

atom_t next_free_atom = first_free_atom;

#ifndef ISABLE_DEBUG
bool debug_eval = false;
bool debug_hashtable = false;
bool debug_gc = false;
bool debug_atom = false;
bool debug_parse = false;
bool debug_sanity = false;
bool debug_string = false;
bool* debug_enabled = &base_loaded;
bool always = true;
#endif

bool enable_trace = false;

#define fatal_error(...) fatal_error_(__func__, __FILE__, __LINE__, __VA_ARGS__)
#define guarantee(p, ...) do{ if(!(p)){ fatal_error("guarantee failed `" #p "': " __VA_ARGS__); } }while(0)
#define guarantee_errno(p, f) guarantee(p, "%s: %s", f, strerror(errno))
#define debug(...) do{ int _debug_res = fprintf(stderr, __VA_ARGS__); guarantee_errno(_debug_res, "fprintf"); }while(0)

#define FRAME_ENTER \
    ASSERT_CODE(\
                char use_FRAME_RETURN_or_FRAME_LEAVE_instead_of_return; \
                const char* parent_frame_func = current_frame_func; \
                current_frame_func = __func__; \
                size_t current_c_frame_count = ++c_frame_count; ) \
    size_t current_c_frame = next_c_term

#define FRAME_ENTER_1(a) FRAME_ENTER; FRAME_TRACK_VAR(a)
#define FRAME_ENTER_2(a, b) FRAME_ENTER_1(a); FRAME_TRACK_VAR(b)
#define FRAME_ENTER_3(a, b, c) FRAME_ENTER_2(a, b); FRAME_TRACK_VAR(c)
#define FRAME_ENTER_4(a, b, c, d) FRAME_ENTER_3(a, b, c); FRAME_TRACK_VAR(d)
#define FRAME_ENTER_5(a, b, c, d, e) FRAME_ENTER_4(a, b, c, d); FRAME_TRACK_VAR(e)

#define ENSURE_INSIDE_FRAME (void)current_c_frame

#define FRAME_TRACK_VAR(name) ENSURE_INSIDE_FRAME; \
    guarantee(next_c_term < MAX_C_TERMS, "C stack too big"); \
    root.c_terms[next_c_term++] = &(name)

#define FRAME_LOCAL(name) ENSURE_INSIDE_FRAME; \
    Term* name = NULL; \
    FRAME_TRACK_VAR(name); \
    name

#define FRAME_LEAVE ENSURE_INSIDE_FRAME; \
    ASSERT_CODE((void)use_FRAME_RETURN_or_FRAME_LEAVE_instead_of_return;) \
    D_SANITY{ \
        guarantee( \
                  current_c_frame <= next_c_term \
                  ASSERT_CODE(&& c_frame_count == current_c_frame_count), \
                  "internal error: c frame mismatch: leaving %s after entering %s", __func__, current_frame_func); \
    } \
    ASSERT_CODE(current_frame_func = parent_frame_func;) \
    ASSERT_CODE(--c_frame_count;) \
    next_c_term = current_c_frame \

#define FRAME_RETURN(type, ret) do{ type _frame_ret = ret; FRAME_LEAVE; return _frame_ret; }while(0)

void* system_alloc(size_t size){
    void* ret = malloc(size);
    if(!ret){
        fprintf(stderr, "fatal error: memory allocation failed: %s\n", strerror(errno));
        BREAKPOINT;
        exit(1);
    }
    return ret;
}

void* system_realloc(void* p, size_t size){
    void* ret = realloc(p, size);
    if(!ret){
        fprintf(stderr, "fatal error: memory allocation failed: %s\n", strerror(errno));
        BREAKPOINT;
        exit(1);
    }
    return ret;
}

void fatal_error_(const char* func, const char* file, int line, char* format, ...){
    va_list argptr;
    va_start(argptr, format);
    int res = fprintf(stderr, "fatal error in %s at %s:%d: ", func, file, line);
    guarantee_errno(res >= 0, "fprintf");
    res = vfprintf(stderr, format, argptr);
    guarantee_errno(res >= 0, "vfprintf");
    res = fprintf(stderr, "\n");
    guarantee_errno(res >= 0, "fprintf");
    va_end(argptr);
    if(current_eval_env && current_eval_env->query){
        trace_term("while evaluating", current_eval_env->query);
    }
    BREAKPOINT;
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

void Streams_close_after_fork(){
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

Stream* Stream_get(int n){
    if(n >= MAX_STREAMS || streams[n].pos > streams[n].alloc_size) {
        fatal_error("invalid stream");
    }
    return &streams[n];
}

Buffer* Buffer_empty(size_t size){
    Buffer* buffer = system_alloc(sizeof(Buffer));
    if(size){
        buffer->alloc_size = size;
        buffer->end = 0;
        buffer->ptr = system_alloc(size + 1);
        buffer->ptr[0] = 0;
    }else{
        buffer->alloc_size = 0;
        buffer->end = 0;
        buffer->ptr = "\0";
    }
    return buffer;
}

Buffer* Buffer_unsafe(size_t size){
    Buffer* buffer = Buffer_empty(size);
    buffer->end = size;
    return buffer;
}

void Buffer_reserve(Buffer* buffer, size_t size){
    if(!buffer->alloc_size && size){
        buffer->ptr = system_alloc(size + 1);
        buffer->ptr[0] = 0;
    }else if(buffer->end > size){
        return;
    }else if(!size){
        buffer->ptr = "\0";
    }else{
        buffer->ptr = system_realloc(buffer->ptr, size + 1);
    }
    buffer->alloc_size = size;
}

void Buffer_shrink(Buffer* buffer){
    Buffer_reserve(buffer, buffer->end);
}

void Buffer_append(Buffer* buffer, char* str, size_t len){
    if(buffer->end + len >= buffer->alloc_size){
        Buffer_reserve(buffer, MAX(buffer->alloc_size * 2, buffer->end + len));
    }
    memcpy(buffer->ptr + buffer->end, str, len);
    buffer->end += len;
    if(buffer->alloc_size){
        buffer->ptr[buffer->end] = 0;
    }
}

void Buffer_append_nt(Buffer* buffer, char* str){
    size_t len = strlen(str);
    Buffer_append(buffer, str, len);
}

Buffer* Buffer_new(char* str, size_t size){
    Buffer* buf = Buffer_empty(0);
    Buffer_append(buf, str, size);
    return buf;
}

Buffer* Buffer_new_nt(char* str){
    Buffer* buf = Buffer_empty(0);
    Buffer_append_nt(buf, str);
    return buf;
}

void Buffer_free(Buffer* buffer){
    if(buffer->alloc_size){
        free(buffer->ptr);
    }
    free(buffer);
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
    Pool* ret = system_alloc(sizeof(Pool));
    ret->sections = 1;
    ret->free = 0;
    ret->terms = system_alloc(sizeof(Term*));
    ret->terms[0] = system_alloc(sizeof(Term) * POOL_SECTION_SIZE);
    return ret;
}

void trace_pool_info(char* str, Pool* p){
    debug("%s: %zu used, %zu available in %zu sections%s\n",
          str,  p->free, p->sections * POOL_SECTION_SIZE,
          p->sections, would_gc ? " (would gc)" :"");
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
    case DICT:
        HashTable_free(term->data.dict);
        break;
    }
}

void Pool_free(Pool* p){
    D_GC{ trace_pool_info("freeing", p); }
    size_t remaining = p->free;
    for(size_t i = 0; i < p->sections; i++){
        for(size_t j = 0; j < POOL_SECTION_SIZE; j++){
            Term_destroy(&p->terms[i][j]);
            remaining--;
            if(!remaining){
                goto outer;
            }
        }
        free(p->terms[i]);
    }
 outer:
    free(p->terms);
    free(p);
}

void Pool_expand(Pool* p){
    D_GC{ trace_pool_info("expanding", p); }
    p->sections++;
    p->terms = system_realloc(p->terms, sizeof(Term*) * p->sections);
    p->terms[p->sections-1] = system_alloc(sizeof(Term) * POOL_SECTION_SIZE);
}

Term* Pool_add_term_expand(Pool* p){
     if(p->free >= p->sections * POOL_SECTION_SIZE){
        Pool_expand(p);
     }
     Term* term = &p->terms[p->free / POOL_SECTION_SIZE][p->free % POOL_SECTION_SIZE];
     p->free++;
     return term;
}

void HashTable_each_term(HashTable* table, term_iterator_t f, void* data){
    for(size_t i = 0; i < table->size; i++){
        if(table->table[i]){
            f(&table->table[i], data);
        }
    }
}

void Pool_pour(Term** term, Pool *p){
    if((*term)->type == MOVED){
        *term = (*term)->data.moved_to;
    }else{
        Term* new = Pool_add_term_expand(p);
        memcpy(new, *term, sizeof(Term));
        (*term)->type = MOVED;
        (*term)->data.moved_to = new;
        *term = new;
        switch(new->type){
        case VAR:
            Pool_pour(&new->data.var.ref, p);
            break;
        case FUNCTOR:
            for(functor_size_t i = 0; i < new->data.functor.size; i++){
                Pool_pour(&new->data.functor.args[i], p);
            }
            break;
        case DICT:
            HashTable_each_term(new->data.dict, (term_iterator_t)Pool_pour, (void*)p);
            break;
        case INTEGER:
        case STRING:
            break;
        case MOVED:
            UNREACHABLE;
        }
    }
}

void each_root(term_iterator_t f, void* data){
    HashTable_each_term(root.globals, f, data);
    HashTable_each_term(root.ops, f, data);
    HashTable_each_term(root.interned, f, data);
    HashTable_each_term(root.atom_names, f, data);
    if(root.nil){ f(&root.nil, data); }
    for(size_t i = 0; i < next_c_term; i++){
        if(*root.c_terms[i]){
            f(root.c_terms[i], data);
        }
    }
}

void gc(Pool** p){
    D_GC{ trace_pool_info("start gc", *p); }
    Pool *new = Pool_new();
    D_GC{ trace_pool_info("new pool", new); }
    each_root((term_iterator_t)Pool_pour, new);
    Pool_free(*p);
    Pool_expand(new);
    *p = new;
    D_GC{ trace_pool_info("finished gc", *p); }
}

void disable_gc(){
    gc_disable_count++;
}

void enable_gc(){
    gc_disable_count--;
}

Term* Pool_add_term_gc(Pool** p){
    if((*p)->free >= (*p)->sections * POOL_SECTION_SIZE){
        if(gc_disable_count){
            would_gc = true;
        }else{
            gc(p);
        }
     }
     Term* term = Pool_add_term_expand(*p);
     return term;
}

void do_nothing(){ }

void sanity_check_term(Term** term, ...){
    Term_render(*term, RENDER_STRICT | RENDER_NO_OP, def_outer_prec, def_outer_prec, false, do_nothing, NULL);
}

void sanity_check_all(){
    static bool checking = false;
    if(checking) return;
    checking = true;
    each_root((term_iterator_t)sanity_check_term, NULL);
    checking = false;
}

Term* Integer(integer_t n){
    Term* term = Pool_add_term_gc(&pool);
    term->type = INTEGER;
    term->data.integer = n;
    return term;
}

int Buffer_cmp(Buffer* a, Buffer* b){
    if(a->end < b->end){
        return -(int)(b->end - a->end);
    }else if(a->end > b->end){
        return a->end - b->end;
    }else{
        for(size_t i = 0; i < a->end; i++){
            if(a->ptr[i] != b->ptr[i]){
                return (int)a->ptr[i] - b->ptr[i];
            }
        }
    }
    return 0;
}

Term* String(char* ptr, size_t size){
    Term* term = Pool_add_term_gc(&pool);
    term->type = STRING;
    term->data.string.ptr = NULL;
    term->data.string.alloc_size = 0;
    term->data.string.end = 0;
    Buffer_append(&term->data.string, ptr, size);
    return term;
}

Term* String_unsafe(size_t size){
    Term* term = Pool_add_term_gc(&pool);
    term->type = STRING;
    term->data.string.ptr = NULL;
    term->data.string.alloc_size = 0;
    term->data.string.end = 0;
    Buffer_reserve(&term->data.string, size);
    term->data.string.end = size;
    term->data.string.ptr[term->data.string.end] = 0;
    return term;
}

Term* String_nt(char* str){
    Term* term = Pool_add_term_gc(&pool);
    term->type = STRING;
    term->data.string.ptr = NULL;
    term->data.string.alloc_size = 0;
    term->data.string.end = 0;
    Buffer_append_nt(&term->data.string, str);
    return term;
}

Term* Atom(atom_t atom){
    if(!atom){
        fatal_error("internal error: atom is null");
    }
    Term* term = Pool_add_term_gc(&pool);
    term->type = FUNCTOR;
    term->data.functor.atom = atom;
    term->data.functor.size = 0;
    term->data.functor.args = NULL;
    return term;
}

Term* Functor_unsafe(atom_t atom, functor_size_t size){
    Term* term = Pool_add_term_gc(&pool);
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
    guarantee(term->type == FUNCTOR, "Functor_set_arg: not a functor");
    guarantee(term->data.functor.size > n, "Functor_set_arg: too few arguments");
    term->data.functor.args[n] = arg;
}

Term* Functor1(atom_t atom, Term* a){
    FRAME_ENTER_1(a);
    Term* term = Functor_unsafe(atom, 1);
    Functor_set_arg(term, 0, a);
    FRAME_RETURN(Term*, term);
}

Term* Functor2(atom_t atom, Term* a, Term* b){
    FRAME_ENTER_2(a, b);
    Term* term = Functor_unsafe(atom, 2);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    FRAME_RETURN(Term*, term);
}

Term* Functor3(atom_t atom, Term* a, Term* b, Term* c){
    FRAME_ENTER_3(a, b, c);
    Term* term = Functor_unsafe(atom, 3);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    FRAME_RETURN(Term*, term);
}

Term* Functor4(atom_t atom, Term* a, Term* b, Term* c, Term* d){
    FRAME_ENTER_4(a, b, c, d);
    Term* term = Functor_unsafe(atom, 4);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    FRAME_RETURN(Term*, term);
}

Term* Functor5(atom_t atom, Term* a, Term* b, Term* c, Term* d, Term* e){
    FRAME_ENTER_5(a, b, c, d, e);
    Term* term = Functor_unsafe(atom, 5);
    Functor_set_arg(term, 0, a);
    Functor_set_arg(term, 1, b);
    Functor_set_arg(term, 2, c);
    Functor_set_arg(term, 3, d);
    Functor_set_arg(term, 4, e);
    FRAME_RETURN(Term*, term);
}

Term* Var(atom_t name){
    Term* term = Pool_add_term_gc(&pool);
    term->type = VAR;
    term->data.var.name = name;
    term->data.var.ref = term;
    return term;
}

bool Var_is_terminal(Term* term){
    return term->type == VAR && term->data.var.ref == term;
}

hash_t hash_byte(uint8_t c, hash_t hash){
    return (hash ^ c) * HASH_PRIME;
}

hash_t hash_string(Buffer* str, hash_t hash){
    for(size_t i = 0; i < str->end; i++){
        hash = hash_byte(str->ptr[i], hash);
    }
    return hash;
}

hash_t hash_integer(integer_t x, hash_t hash){
    char* c = (char*)&x;
    for(size_t i = 0; i < sizeof(x); i++){
        hash = hash_byte(c[i], hash);
    }
    return hash;
}

hash_t hash_atom(atom_t atom, hash_t hash){
    char* c = (char*)&atom;
    for(size_t i = 0; i < sizeof(atom); i++){
        hash = hash_byte(c[i], hash);
    }
    return hash;
}

hash_t hash_rec(Term* term, hash_t hash){
    term = chase(term);
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
        return hash_string(&term->data.string, hash);
    case VAR:
        fatal_error("Cannot hash variable '%s'", term->data.var.name);
    case DICT:
        fatal_error("unimplemented: hash dict");
    case MOVED:
        fatal_error("Cannot hash a moved term");
    }
    UNREACHABLE;
}

hash_t hash(Term* term){
    uint32_t hash = HASH_INIT;
    return hash_rec(term, hash);
}

atom_t intern(Term* str){
    FRAME_ENTER_1(str);
    assert(str->type == STRING, "cannot intern non-string");
    FRAME_LOCAL(term) = chase(HashTable_get(root.interned, str));
    if(!Var_is_terminal(term)){
        guarantee(is_Atom(term), "interned term is not an atom");
        D_ATOM{
            debug("already interned `%s' as %lu\n", str->data.string.ptr, term->data.functor.atom);
        }
        FRAME_RETURN(atom_t, term->data.functor.atom);
    }
    atom_t atom = next_free_atom++;
    FRAME_LOCAL(tatom) = Atom(atom);
    set_var(term, tatom);
    FRAME_LOCAL(rev) = HashTable_get(root.atom_names, tatom);
    set_var(rev, str);
    D_ATOM{
        debug("interning %s as %lu\n", str->data.string.ptr, atom);
    }
    FRAME_RETURN(atom_t, atom);
}

atom_t intern_nt(char* string){
    FRAME_ENTER;
    FRAME_LOCAL(s) = String_nt(string);
    FRAME_RETURN(atom_t, intern(s));
}

void intern_prim(char* string, atom_t atom){
    FRAME_ENTER;
    FRAME_LOCAL(str) = String_nt(string);
    FRAME_LOCAL(term) = HashTable_get(root.interned, str);
    if(!Var_is_terminal(term)){
        fatal_error("prim '%s' already exists", string);
    }
    set_var(term, Atom(atom));
    FRAME_LOCAL(rev) = HashTable_get(root.atom_names, term);
    set_var(rev, str);
    D_ATOM{
        debug("interning primitve %s as %lu\n", string, atom);
    }
    FRAME_LEAVE;
}

Term* Spec(atom_t atom, int size){
    FRAME_ENTER;
    FRAME_LOCAL(tatom) = Atom(atom);
    FRAME_LOCAL(tsize) = Integer(size);
    FRAME_RETURN(Term*, Functor2(atom_slash, tatom, tsize));
}

Term* chase(Term* term){
    while(term->type == VAR && term->data.var.ref != term){
        term = term->data.var.ref;
    }
    return term;
}

Term* atom_to_String(atom_t atom){
    Term atom_term;
    atom_term.type = FUNCTOR;
    atom_term.data.functor.atom = atom;
    atom_term.data.functor.size = 0;
    atom_term.data.functor.args = NULL;
    Term* term = HashTable_find(root.atom_names, &atom_term);
    if(!term || Var_is_terminal(term)){
        char buf[20];
        snprintf(buf, 20, "?atom_%lu?", atom);
        return String_nt(buf);
    }
    guarantee(term->type == STRING, "internal error: atom names table contains non-string");
    return term;
}

Buffer* atom_to_string(atom_t atom){
    return &atom_to_String(atom)->data.string;
}

int op_type_arg_size(atom_t type){
    switch(type){
    case atom_xf:
    case atom_yf:
    case atom_fx:
    case atom_fy:
        return 1;
    case atom_xfx:
    case atom_yfx:
    case atom_xfy:
        return 2;
    default:
        fatal_error("invalid op type");
        UNREACHABLE;
    }
}

void op_type(integer_t prec, atom_t atom, integer_t *left, integer_t *right){
    switch(atom){
    case atom_xf:
    case atom_xfx:
    case atom_xfy:
        *left = prec; break;
    case atom_yf:
    case atom_yfx:
        *left = prec -1; break;
    case atom_fx:
    case atom_fy:
        *left = 0; break;
    default:
        fatal_error("invalid op spec");
    }
    switch(atom){
    case atom_fx:
    case atom_xfx:
    case atom_yfx:
        *right = prec; break;
    case atom_fy:
    case atom_xfy:
        *right = prec -1; break;
    case atom_xf:
    case atom_yf:
        *right = 0;
    default:
        fatal_error("invalid op spec");
    }
}

bool Functor_render_op(Term* term, int render_flags, int left_prec, int right_prec, bool in_list, renderer_t write, void* data){
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
            Term_render(car, render_flags, def_outer_prec, def_outer_prec, true, write, data);
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
                Term_render(cdr, render_flags, def_outer_prec, def_outer_prec, true, write, data);
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
        inner_prec = Term_integer(args[0]);
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
        left_prec = def_outer_prec;
        right_prec = def_outer_prec;
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
        if (render_flags & RENDER_NO_OP || !Functor_render_op(term, render_flags, left_prec, right_prec, in_list, write, data)) {
            Buffer* name = atom_to_string(term->data.functor.atom);
            write(data, name->ptr, name->end);
            if(term->data.functor.size){
                write(data, "(", 1);
                for(int i = 0; i < term->data.functor.size; i++){
                    Term_render(term->data.functor.args[i], render_flags, def_outer_prec, def_outer_prec, true, write, data);
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

Buffer* Term_show(Term* term, int render_flags){
    Buffer* buffer = Buffer_empty(0);
    Term_render(term, render_flags, def_outer_prec, def_outer_prec, false, (renderer_t)Buffer_append, buffer);
    Buffer_shrink(buffer);
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
    Buffer* buffer = Term_show(term, 0);
    debug(": %s\n", short_snippet(buffer->ptr, buf, sizeof buf));
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

Term* Nil(){
    if(!root.nil){
        root.nil = Atom(atom_nil);
    }
    return root.nil;
}

Term* List_head(Term* list){
    Term** args = Functor_get(list, atom_cons, 2);
    if(!args){
        trace_term("list", list);
        fatal_error("expected non-empty list");
    }
    return args[0];
}

Term* List_tail(Term* list){
    Term** args = Functor_get(list, atom_cons, 2);
    if(!args){
        trace_term("list", list);
        fatal_error("expected non-empty list");
    }
    return args[1];
}

integer_t List_length(Term* list){
    integer_t ret = 0;
    for(; !Atom_eq(list, atom_nil); list = List_tail(list)){
        ret++;
    }
    return ret;
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
        return !Buffer_cmp(&a->data.string, &b->data.string);
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
    case DICT:
        fatal_error("unimplemented: exact_eq dict");
    }
    UNREACHABLE;
}

Term* Assoc_get(Term** assoc, Term* key){
    FRAME_ENTER_1(key);
    for(Term* list = *assoc; !Atom_eq(list, atom_nil); list = List_tail(list)){
        Term** args = Functor_get(List_head(list), atom_colon, 2);
        if(!args) fatal_error("Not an assoc list");
        if(Term_exact_eq(key, args[0])){
            D_HASHTABLE{
                if(Atom_eq(args[1], atom_nil)){
                    trace_term("hash collision", key);
                }
            }
            FRAME_RETURN(Term*, args[1]);
        }
    }
    FRAME_LOCAL(var) = Var(atom_underscore);
    FRAME_LOCAL(pair) = Functor2(atom_colon, key, var);
    *assoc = Functor2(atom_cons, pair, *assoc);
    FRAME_RETURN(Term*, var);
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
            return chase(args[1]);
        }
    }
    return NULL;
}

Term* HashTable_get(HashTable* table, Term* key){
    FRAME_ENTER_1(key);
    hash_t hkey = hash(key);
    Term** assoc = &table->table[hkey % table->size];
    D_HASHTABLE{
        debug("hashtable %p get:\n", (void*)table);
        trace_term("key (hash %u)", key, hkey);
        trace_term("assoc", *assoc);
    }
    if(!*assoc){
        *assoc = Nil();
    }
    FRAME_LOCAL(val) = Assoc_get(assoc, key);
    D_HASHTABLE{
        trace_term("val", val);
    }
    FRAME_RETURN(Term*, val);
}

void HashTable_append(HashTable* table, Term* key, Term* val){
    FRAME_ENTER_2(key, val);
    FRAME_LOCAL(list) = HashTable_get(table, key);
    FRAME_LOCAL(tail) = NULL;
    for(list = chase(list); list->type != VAR; list = chase(List_tail(list))){ }
    set_var(list, Functor2(atom_cons, val, Var(atom_underscore)));
    D_HASHTABLE{
        debug("hashtable %p: append\n", (void*)table);
        trace_term("key", key);
        trace_term("val", val);
    }
    FRAME_LEAVE;
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
    Term_render(term, RENDER_DEFAULT, def_outer_prec, def_outer_prec, false, (renderer_t)render_fprintf, stdout);
}

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

Term* Term_copy_rec(Term* term, HashTable* vars){
    term = chase(term);
    switch(term->type){
    case INTEGER:
    case STRING:
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
        Term* copy = HashTable_get(vars, Integer((integer_t)term)); //TODO: don't cast pointer to integer
        assert(copy->type == VAR && (copy->data.var.name == atom_underscore || copy->data.var.name == term->data.var.name), "invalid variable in hashtable");
        copy->data.var.name = term->data.var.name;
        return copy;
    }
    case DICT:
        fatal_error("unimplemented: copyrec dict");
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

Term* Dict(size_t size){
    Term* term = Pool_add_term_gc(&pool);
    term->type = DICT;
    term->data.dict = HashTable_new(size);
    return term;
}

Term* Term_copy(Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(vars) = Dict(COLLISION_HASHTABLE_SIZE);
    FRAME_RETURN(Term*, Term_copy_rec(term, vars->data.dict));
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

void add_undo_var(Term* stack, Term* var){
    FRAME_ENTER_2(stack, var);
    Term** args = Functor_get(stack, atom_frame, 3);
    if(!args){
        FRAME_LEAVE;
        return;
    }
    FRAME_LOCAL(undo_vars) = args[1];
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
    Term** args = Functor_get(stack, atom_frame, 3);
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

int stack_depth(eval_env_t* eval){
    int ret = 0;
    for(Term* stack = eval->stack; !Atom_eq(stack, atom_empty); ret++){
        if(Atom_eq(stack, atom_c_land)){
            eval = eval->prev_eval;
            stack = eval->stack;
        }else{
            Term** args = Functor_get(stack, atom_frame, 3);
            assert(args, "invalid frame");
            stack = args[2];
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

bool stack_push(eval_env_t* eval, atom_t atom, functor_size_t size, Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(spec) = Spec(atom, size);
    Term* rules = HashTable_find(root.globals, spec);
    if(!rules){
        FRAME_RETURN(bool, error("No such predicate '%s'/%u", atom_to_string(atom)->ptr, size));
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
        FRAME_RETURN(bool, error("No rules for predicate '%s/%u'", atom_to_string(atom), size));
    }
    eval->stack = Functor3(atom_frame, branches, Nil(), eval->stack);
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
    Term** args = Functor_get(eval->stack, atom_frame, 3);
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
        eval->stack = parent;
        eval->next_query = Atom(atom_true);
        return true;
    }else{
        reset_undo_vars(*vars);
        *vars = Nil();
        Term** car_cdr = Functor_get(chase(*branches), atom_cons, 2);
        if(car_cdr){
            Term** cadr_args = Functor_get(car_cdr[1], atom_frame, 3);
            if(Atom_eq(car_cdr[1], atom_nil) ||
               (cadr_args && Atom_eq(cadr_args[1], atom_drop))){
                *vars = Atom(atom_drop);
            }
            *branches = car_cdr[1];
            eval->next_query = car_cdr[0];
            return true;
        }else{
            eval->stack = parent;
            return stack_next(eval, false);
        }
    }
}

void set_var(Term* a, Term* b){
    assert(a->type == VAR, "not a variable");
    assert(a->data.var.ref == a, "variable is already set");
    D_EVAL{
        char* name = atom_to_string(a->data.var.name)->ptr;
        if(name[0] != '_'){
            trace_term("set_var `%s'", b, name);
        }
    }
    if(enable_trace && base_loaded){
        trace_info_t trace;
        trace_record(&trace, Functor2(atom_eq, a, b), current_eval_env);
        trace_show(&trace, true);
    }
    a->data.var.ref = b;
    if(current_eval_env){
        add_undo_var(current_eval_env->stack, a);
    }
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
        UNREACHABLE;
    }
    UNREACHABLE;
}

void Var_push(Term** var, Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(ret) = Var(atom_underscore);
    bool ok = unify(*var, Functor2(atom_cons, term, ret));
    guarantee(ok, "internal error: failed to unify");
    *var = ret;
    FRAME_LEAVE;
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

integer_t Term_integer(Term* term){
    term = chase(term);
    if(term->type != INTEGER){
        fatal_error("expected integer");
    }
    return term->data.integer;
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
    int stream = Term_integer(args[0]);
    Buffer* str = Term_string(args[1]);
    ssize_t res = write(streams[stream].fd, str->ptr, str->end);
    if(res >= 0){
        return true;
    }else{
        return error("warning: write failed: %s\n", strerror(errno));
    }
}

bool prim_read_string(Term** args){
    integer_t stream_id = Term_integer(args[0]);
    integer_t max = Term_integer(args[1]);
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
            return error("warning: read failed: %s\n", strerror(errno));
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
            term->data.string.ptr[n++] = Term_integer(List_head(list));
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
    Term* atom = chase(args[0]);
    Term* string = chase(args[1]);
    if(is_Atom(atom)){
        Term* s = atom_to_String(atom->data.functor.atom);
        bool ret = unify(string, s);
        return ret;
    }else if(string->type == STRING){
        bool ret = unify(atom, Atom(intern(string)));
        return ret;
    }else{
        fatal_error("invalid arguments to atom_string");
        UNREACHABLE;
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
    for(; !Atom_eq(rules, atom_nil); rules = chase(List_tail(rules))){
        Term_print(List_head(rules));
        printf("\n");
    }
    FRAME_RETURN(bool, true);
}

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

void pop_eval_env(bool success){
    FRAME_ENTER;
    FRAME_LOCAL(stack) = current_eval_env->stack;
    current_eval_env = current_eval_env->prev_eval;
    FRAME_LOCAL(next) = NULL;
    D_EVAL{ debug("pop eval: %s\n", success ? "true" : "fail"); }
    while(!Atom_eq(stack, atom_c_land) && !Atom_eq(stack, atom_empty)){
        Term** args = Functor_get(stack, atom_frame, 3);
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

bool eval_query(Term* query){
    FRAME_ENTER_1(query);
    FRAME_LOCAL(term) = Nil();
    eval_env_t eval;
    eval.query = NULL; FRAME_TRACK_VAR(eval.query);
    eval.next_query = NULL; FRAME_TRACK_VAR(eval.next_query);
    if(current_eval_env){
        eval.stack = Functor3(atom_frame, Atom(atom_true), Nil(), Atom(atom_c_land));
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
            FRAME_RETURN(bool, error("Cannot eval integer %ld", term->data.integer));
        case STRING:
            pop_eval_env(false);
            FRAME_RETURN(bool, error("Cannot eval string \"%s\"", term->data.string));
        case VAR:
            pop_eval_env(false);
            FRAME_RETURN(bool, error("Cannot eval unbound variable '%s'", atom_to_string(term->data.var.name)));
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
        }
    }
}

bool issymbol(char c){
    return !isalnum(c) && !isspace(c) && !strchr("()[],'_\"%", c) && isprint(c);
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

Term* parse_atomic(char** str, HashTable* vars){
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
    if(isalpha(*pos) || *pos == '_'){
        if(isupper(*pos) || *pos == '_'){
            var = true;
        }
        while(isalpha(*pos) || isdigit(*pos) || *pos == '_'){ pos++; }
        end = pos;
    }else if(*pos == '\''){
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
    Term* atom = parse_atomic(&pos, vars);
    if(!atom) return NULL;
    if(is_Atom(atom) && HashTable_find(root.ops, atom)){
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

Term* parse_term_partial(char** str){
    FRAME_ENTER;
    FRAME_LOCAL(vars) = Dict(PARSE_VARS_HASHTABLE_SIZE);
    FRAME_RETURN(Term*, parse_term_vars(str, vars->data.dict, NULL));
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
        FRAME_RETURN(bool, eval_query(q));
    }
    HashTable_append(root.globals, Spec(term->data.functor.atom, term->data.functor.size), term);
    FRAME_RETURN(bool, true);
}

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

void load_base(){
    load_file(LIB_PATH "/base.pl");

    base_loaded = true;
}

void list_vars(Term* term, HashTable* vars){
    FRAME_ENTER_1(term);
    term = chase(term);
    switch(term->type){
    case VAR:
        if(atom_to_string(term->data.var.name)->ptr[0] == '_') return;
        FRAME_LOCAL(val) = HashTable_get(vars, Integer((integer_t)term));
        if(Var_is_terminal(val)){
            set_var(val, term);
        }
        break;
    case FUNCTOR:
        for(functor_size_t i = 0; i < term->data.functor.size; i++){
            list_vars(term->data.functor.args[i], vars);
        }
        break;
    case DICT:
        fatal_error("unimplemented: list_vars dict");
    case MOVED:
    case INTEGER:
    case STRING:
    default:
        (void)0;
    }
    FRAME_LEAVE;
}

Term* vars_of(Term* term){
    disable_gc();
    FRAME_ENTER_1(term);
    HashTable* vars = HashTable_new(PARSE_VARS_HASHTABLE_SIZE);
    list_vars(term, vars);
    FRAME_LOCAL(list) = Nil();
    FRAME_LOCAL(assoc) = NULL;
    FRAME_LOCAL(var) = NULL;
    for(size_t i = 0; i < vars->size; i++){
        assoc = vars->table[i];
        if(assoc){
            for(; !Atom_eq(assoc, atom_nil); assoc = List_tail(assoc)){
                Term** args = Functor_get(List_head(assoc), atom_colon, 2);
                var = Var(args[1]->data.var.ref->data.var.name);
                list = Functor2(atom_cons, Functor2(atom_eq, var, args[1]), list);
            }
        }
    }
    HashTable_free(vars);
    enable_gc();
    FRAME_RETURN(Term*, list);
}

void eval_interactive(Term* term){
    FRAME_ENTER_1(term);
    FRAME_LOCAL(vars) = vars_of(term);
    if(eval_query(term)){
        if(Atom_eq(vars, atom_nil)){
            printf("yep.\n");
        }else{
            for(; !Atom_eq(vars, atom_nil); vars = List_tail(vars)){
                Buffer* buffer = Term_show(List_head(vars), RENDER_NO_CHASE);
                printf("%s.\n", buffer->ptr);
                Buffer_free(buffer);
            }
        }
    }else{
        printf("nope.\n");
    }
    FRAME_LEAVE;
}

void eval_stdin(char* prompt, void (*eval)(Term*)){
    Buffer* buffer = Buffer_empty(4096);
    bool term = isatty(0);
    if(term){
        printf("%s", prompt);
        fflush(stdout);
    }
    while(true){
        if(buffer->end == buffer->alloc_size){
            Buffer_reserve(buffer, buffer->alloc_size * 2);
        }
        ssize_t n = read(0, buffer->ptr + buffer->end, buffer->alloc_size - buffer->end);
        if(n < 0){
            fatal_error("read error: %s", strerror(errno));
        }
        if(!n){
            if(buffer->end){
                fatal_error("could not parse: %s", buffer->ptr);
            }
            if(term){
                printf("\n");
            }
            return;
        }
        buffer->end += n;
        buffer->ptr[buffer->end] = 0;

        char* pos = buffer->ptr;
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
            size_t remaining = buffer->end - (next - buffer->ptr);
            memmove(buffer->ptr, next, remaining + 1);
            buffer->end = remaining;
        }
    }
}

int main(int argc, char** argv){
    (void)argc;
    char** args = argv + 1;
    char* arg;
    char* file = NULL;
    char* eval = NULL;
    char usage[] = "usage: poorlog [FILE] [-e EXPR] [-t] [-dparse] [-deval] [-dhashtable] [-dnogc] [-datom] [-dbase] [-dsanity]";
    bool please_debug_sanity = false;
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
        case 't':
            enable_trace = true;
            break;
        case 'd':
            if(!strcmp(arg+2, "sanity")) please_debug_sanity = true; else
#ifdef ISABLE_DEBUG
            fatal_error("Debug modes are disabled. build without -DISABLE_DEBUG to enable.");
            break;
#else
            if(!strcmp(arg+2, "parse")) debug_parse = true; else
            if(!strcmp(arg+2, "eval")) debug_eval = true; else
            if(!strcmp(arg+2, "hashtable")) debug_hashtable = true; else
            if(!strcmp(arg+2, "gc")) debug_gc = true; else
            if(!strcmp(arg+2, "atom")) debug_atom = true; else
            if(!strcmp(arg+2, "string")) debug_string = true; else
            if(!strcmp(arg+2, "base")) debug_enabled = &always;
            else fatal_error("unknown debug mode: %s", arg+2);
            break;
#endif
        default:
            fatal_error("unknown argument: %s\n%s\n", arg, usage);
            exit(1);
        }
    }

    pool = Pool_new();

    root.globals = HashTable_new(GLOBALS_SIZE);
    root.ops = HashTable_new(OPS_HASHTABLE_SIZE);
    root.interned = HashTable_new(INTERNED_TABLE_SIZE);
    root.atom_names = HashTable_new(INTERNED_TABLE_SIZE);
    Streams_init();

#ifndef ISABLE_DEBUG
    debug_sanity = please_debug_sanity;
#endif

#define DEFINE_ATOM(name, string) intern_prim(string, name);

    EACH_BUILTIN_ATOM(DEFINE_ATOM)

    load_base();

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
