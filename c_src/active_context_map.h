//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#pragma once

#include <stdbool.h>
#include "erl_nif.h"
#include <EGL/egl.h>

#define MAX_ACTIVE_CONTEXTS 32

typedef struct {
    ErlNifPid pid;
    EGLContext context;
} PidContextEntry;

typedef struct {
    PidContextEntry entries[MAX_ACTIVE_CONTEXTS];
    size_t count;
} ActiveContextMap;

void active_context_map_init(ActiveContextMap* map);

bool active_context_map_add(ActiveContextMap* map, const ErlNifPid* pid, EGLContext context);
bool active_context_map_remove_by_pid(ActiveContextMap* map, const ErlNifPid* pid);
bool active_context_map_remove_by_context(ActiveContextMap* map, EGLContext context);

size_t active_context_map_size(const ActiveContextMap* map);

EGLContext* active_context_map_find_by_pid(ActiveContextMap* map, const ErlNifPid* pid);
ErlNifPid* active_context_map_find_by_context(ActiveContextMap* map, EGLContext context);
