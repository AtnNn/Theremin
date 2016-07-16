#include "genheader.h"

#include <string.h>
#include <stdlib.h>

#include "alloc.h"

EXPORT_PUBLIC
typedef struct Buffer {
    size_t alloc_size;
    size_t end;
    char* ptr;
} Buffer;

#define MAX(a,b) ((a) < (b) ? b : a)

EXPORT
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

EXPORT
Buffer* Buffer_unsafe(size_t size){
    Buffer* buffer = Buffer_empty(size);
    buffer->end = size;
    return buffer;
}

EXPORT
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

EXPORT
void Buffer_shrink(Buffer* buffer){
    Buffer_reserve(buffer, buffer->end);
}

EXPORT
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

EXPORT
void Buffer_append_nt(Buffer* buffer, char* str){
    size_t len = strlen(str);
    Buffer_append(buffer, str, len);
}

EXPORT
Buffer* Buffer_new(char* str, size_t size){
    Buffer* buf = Buffer_empty(0);
    Buffer_append(buf, str, size);
    return buf;
}

EXPORT
Buffer* Buffer_new_nt(char* str){
    Buffer* buf = Buffer_empty(0);
    Buffer_append_nt(buf, str);
    return buf;
}

EXPORT
void Buffer_free(Buffer* buffer){
    if(buffer->alloc_size){
        free(buffer->ptr);
    }
    free(buffer);
}
