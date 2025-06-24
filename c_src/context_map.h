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
#include <EGL/egl.h>
#include "command_executor.h"

typedef struct {
    EGLContext context;
    CommandExecutor executor;
} ContextMapEntry;

typedef struct {
    ContextMapEntry* entries;
    size_t capacity;
    size_t size;
} ContextMap;

ContextMap* context_map_create(size_t initial_capacity);
void context_map_destroy(ContextMap* map);

CommandExecutor* context_map_put(ContextMap* map, EGLContext context);
CommandExecutor* context_map_get(ContextMap* map, EGLContext context);
bool context_map_erase(ContextMap* map, EGLContext context);
