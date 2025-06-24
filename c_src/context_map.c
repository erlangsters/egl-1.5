//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#include "context_map.h"

ContextMap* context_map_create(size_t initial_capacity) {
    if (initial_capacity == 0) {
        return NULL;
    }

    ContextMap* map = malloc(sizeof(ContextMap));
    if (!map) {
        return NULL;
    }

    map->entries = calloc(initial_capacity, sizeof(ContextMapEntry));
    if (!map->entries) {
        free(map);
        return NULL;
    }

    map->capacity = initial_capacity;
    map->size = 0;

    for (size_t i = 0; i < initial_capacity; i++) {
        map->entries[i].context = EGL_NO_CONTEXT;
    }

    return map;
}

void context_map_destroy(ContextMap* map) {
    if (!map) return;

    free(map->entries);
    free(map);
}

CommandExecutor* context_map_put(ContextMap* map, EGLContext context) {
    if (!map || context == EGL_NO_CONTEXT) {
        return NULL;
    }

    for (size_t i = 0; i < map->size; i++) {
        if (map->entries[i].context == context) {
            return &map->entries[i].executor;
        }
        if (map->entries[i].context == EGL_NO_CONTEXT) {
            map->entries[i].context = context;
            memset(&map->entries[i].executor, 0, sizeof(CommandExecutor));
            return &map->entries[i].executor;
        }
    }

    if (map->size >= map->capacity) {
        size_t new_capacity = map->capacity * 2;
        ContextMapEntry* new_entries = realloc(map->entries, new_capacity * sizeof(ContextMapEntry));
        if (!new_entries) {
            return NULL;
        }

        map->entries = new_entries;
        for (size_t i = map->capacity; i < new_capacity; i++) {
            map->entries[i].context = EGL_NO_CONTEXT;
        }
        map->capacity = new_capacity;
    }

    map->entries[map->size].context = context;
    memset(&map->entries[map->size].executor, 0, sizeof(CommandExecutor));
    CommandExecutor* executor = &map->entries[map->size].executor;
    map->size++;

    return executor;
}

CommandExecutor* context_map_get(ContextMap* map, EGLContext context) {
    if (!map || context == EGL_NO_CONTEXT) {
        return NULL;
    }

    for (size_t i = 0; i < map->size; i++) {
        if (map->entries[i].context == context) {
            return &map->entries[i].executor;
        }
    }
    return NULL;
}

bool context_map_erase(ContextMap* map, EGLContext context) {
    if (!map || context == EGL_NO_CONTEXT) {
        return false;
    }

    for (size_t i = 0; i < map->size; i++) {
        if (map->entries[i].context == context) {
            map->entries[i].context = EGL_NO_CONTEXT;
            return true;
        }
    }

    return false;
}
