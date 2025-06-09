#include "commands_executor.h"
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include <stdio.h>

static pthread_t my_commands_executor;
static pthread_mutex_t my_command_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t my_command_ready = PTHREAD_COND_INITIALIZER;
static pthread_cond_t my_command_done = PTHREAD_COND_INITIALIZER;

static ERL_NIF_TERM (*my_command_function)(ErlNifEnv*, int, const ERL_NIF_TERM[]) = NULL;
static ErlNifEnv* my_command_args_1 = NULL;
static int my_command_args_2 = 0;
static ERL_NIF_TERM** my_command_args_3 = NULL;
static ERL_NIF_TERM my_command_result;

static int my_command_finished = 0;
/* Add a flag so we can exit cleanly if needed */
static bool my_should_terminate = false;

void* my_commands_executor_function(void* arg) {
    while (true) {
        pthread_mutex_lock(&my_command_mutex);
        
        /* Wait as long as there is no command AND we are not terminating. */
        while (!my_should_terminate && my_command_function == NULL) {
            pthread_cond_wait(&my_command_ready, &my_command_mutex);
        }
        // printf("my_commands_executor_function:aaa\n");

        /* If we've been told to stop, break out of the loop. */
        if (my_should_terminate) {
            pthread_mutex_unlock(&my_command_mutex);
            break;
        }

        // printf("my_commands_executor_function:bbb\n");
        // printf("my_command_function: %p\n", my_command_function);

        /* Execute command (now that my_command_function is non-null). */
        my_command_result = my_command_function(
            my_command_args_1,
            my_command_args_2,
            (const ERL_NIF_TERM**)my_command_args_3
        );
        // printf("my_commands_executor_function:ccc\n");

        my_command_function = NULL;
        my_command_finished = 1;
        pthread_cond_signal(&my_command_done);
        pthread_mutex_unlock(&my_command_mutex);
    }

    return NULL;
}

bool my_create_executor_if_not_any(const ErlNifPid* pid) {
    /* For simplicity, ignoring any existing thread checks here. */
    if (pthread_create(&my_commands_executor, NULL, my_commands_executor_function, NULL) != 0) {
        return false;
    }
    return true;
}

bool my_destroy_executor_if_any(const ErlNifPid* pid) {
    pthread_mutex_lock(&my_command_mutex);
    my_should_terminate = true;
    pthread_cond_signal(&my_command_ready);
    pthread_mutex_unlock(&my_command_mutex);

    pthread_join(my_commands_executor, NULL);
    return true;
}

bool my_execute_command_if_executor(
    ErlNifPid* pid,
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    ERL_NIF_TERM* argv[],
    ERL_NIF_TERM* result
) {
    // printf("my_execute_command_if_executor:begin\n");
    pthread_mutex_lock(&my_command_mutex);
    my_command_function = function;
    my_command_args_1 = env;
    my_command_args_2 = argc;
    my_command_args_3 = argv;
    my_command_finished = 0;

    // printf("my_execute_command_if_executor:aaa\n");
    pthread_cond_signal(&my_command_ready);
    // printf("my_execute_command_if_executor:bbb\n");

    while (!my_command_finished) {
        // printf("my_execute_command_if_executor:ccc\n");
        pthread_cond_wait(&my_command_done, &my_command_mutex);
    }
    // printf("my_execute_command_if_executor:dd\n");

    *result = my_command_result;
    pthread_mutex_unlock(&my_command_mutex);

    return true;
}