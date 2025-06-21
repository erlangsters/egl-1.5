//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#include "command_executor.h"

static void* command_executor_function(void* arg) {
    CommandExecutor* executor = (CommandExecutor*)arg;

    while (true) {
        pthread_mutex_lock(&executor->mutex);

        /* Wait as long as there is no command AND we are not terminating. */
        while (!executor->should_terminate && executor->command_function == NULL) {
            pthread_cond_wait(&executor->command_ready, &executor->mutex);
        }

        /* If we've been told to stop, break out of the loop. */
        if (executor->should_terminate) {
            pthread_mutex_unlock(&executor->mutex);
            break;
        }

        /* Execute command */
        executor->command_result = executor->command_function(
            executor->command_env,
            executor->command_argc,
            (const ERL_NIF_TERM**)executor->command_argv
        );

        executor->command_function = NULL;
        executor->command_finished = 1;
        pthread_cond_signal(&executor->command_done);
        pthread_mutex_unlock(&executor->mutex);
    }

    return NULL;
}

bool command_executor_init(CommandExecutor* executor) {
    executor->thread = 0;
    executor->mutex = (pthread_mutex_t)PTHREAD_MUTEX_INITIALIZER;
    executor->command_ready = (pthread_cond_t)PTHREAD_COND_INITIALIZER;
    executor->command_done = (pthread_cond_t)PTHREAD_COND_INITIALIZER;

    executor->command_function = NULL;
    executor->command_env = NULL;
    executor->command_argc = 0;
    executor->command_argv = NULL;

    executor->command_finished = 0;
    executor->should_terminate = false;

    if (pthread_create(&executor->thread, NULL, command_executor_function, executor) != 0) {
        return false;
    }
    return true;
}

bool command_executor_destroy(CommandExecutor* executor) {
    pthread_mutex_lock(&executor->mutex);
    executor->should_terminate = true;
    pthread_cond_signal(&executor->command_ready);
    pthread_mutex_unlock(&executor->mutex);

    pthread_join(executor->thread, NULL);
    return true;
}

void command_executor_execute(
    CommandExecutor* executor,
    ErlNifPid* pid,
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    ERL_NIF_TERM* argv[],
    ERL_NIF_TERM* result
) {
    pthread_mutex_lock(&executor->mutex);
    executor->command_function = function;
    executor->command_env = env;
    executor->command_argc = argc;
    executor->command_argv = argv;
    executor->command_finished = 0;

    pthread_cond_signal(&executor->command_ready);

    while (!executor->command_finished) {
        pthread_cond_wait(&executor->command_done, &executor->mutex);
    }

    *result = executor->command_result;
    pthread_mutex_unlock(&executor->mutex);
}
