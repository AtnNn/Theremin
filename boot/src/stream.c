#include "genheader.h"

#include "stream.h"

#if HEADER_ONLY

#include <stddef.h>

typedef struct Stream {
    int fd;
    char* buf;
    size_t pos;
    size_t size;
    size_t alloc_size;
} Stream;

#endif

#include <unistd.h>
#include <string.h>

#include "error.h"
#include "settings.h"
#include "debug.h"
#include "alloc.h"

static Stream streams[MAX_STREAMS];
static int free_stream = 0;

HEADER_DECLARE
void Streams_init(){
    free_stream = 0;
    for(int n = 0; n < MAX_STREAMS; n++){
        streams[n].fd = n + 1;
        streams[n].pos = 1;
        streams[n].alloc_size = 0;
    }
}

HEADER_DECLARE
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

HEADER_DECLARE
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

HEADER_DECLARE
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

HEADER_DECLARE
Stream* Stream_get(int n){
    if(n >= MAX_STREAMS || streams[n].pos > streams[n].alloc_size) {
        fatal_error("invalid stream");
    }
    return &streams[n];
}
