#include <stdbool.h>
#include <erl_nif.h>

bool my_create_executor_if_not_any(const ErlNifPid* pid);
bool my_destroy_executor_if_any(const ErlNifPid* pid);
bool my_execute_command_if_executor(
    ErlNifPid* pid,
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    ERL_NIF_TERM* argv[],
    ERL_NIF_TERM* result
);
