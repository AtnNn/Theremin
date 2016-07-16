#include "genheader.h"

#include "process.h"

#if HEADER_ONLY

#include "term.h"

#endif

#include <string.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
int kill(pid_t, int);

#include "error.h"
#include "stream.h"
#include "utils.h"
#include "gc.h"
#include "string.h"
#include "list.h"
#include "eval.h"

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

HEADER_DECLARE
bool prim_process_create(Term** args){
    disable_gc();
    char* command_path = String_pack_buf(args[0])->ptr;
    char* command_args[256];
    command_args[0] = command_path;
    size_t n = 1;
    for(Term* list = args[1]; !Atom_eq(list, atom_nil); list = List_tail(list)){
        guarantee(n < sizeof(command_args) - 1, "too many arguments for process");
        command_args[n] = String_pack_buf(List_head(list))->ptr;
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

HEADER_DECLARE
bool prim_kill_process(Term** args){
    Term* pid = chase(args[0]);
    if(pid->type != INTEGER){
        fatal_error("kill_process expects an integer");
    }
    int res = kill(pid->data.integer, SIGTERM);
    guarantee_errno(res >= 0, "kill");
    return true;
}
