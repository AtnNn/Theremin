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

#include "settings.h"
#include "buffer.h"
#include "alloc.h"
#include "debug.h"
#include "term.h"
#include "hashtable.h"
#include "atom.h"
#include "hash.h"
#include "alloc.h"
#include "debug.h"
#include "gc.h"
#include "roots.h"
#include "error.h"
#include "roots.h"
#include "frame.h"
#include "assert.h"
#include "render.h"
#include "list.h"
#include "eval.h"
#include "utils.h"
#include "stream.h"
#include "parse.h"
#include "vars.h"

void load_base(){
    load_file(LIB_PATH "/base.pl");

    base_loaded = true;
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
            set_tracing(true);
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

    gc_init();

    root.globals = HashTable_new(GLOBALS_SIZE);
    root.ops = HashTable_new(OPS_HASHTABLE_SIZE);
    root.interned = HashTable_new(INTERNED_TABLE_SIZE);
    root.atom_names = HashTable_new(INTERNED_TABLE_SIZE);
    Streams_init();

#ifndef ISABLE_DEBUG
    debug_sanity = please_debug_sanity;
#endif

    define_builtin_atoms();

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
