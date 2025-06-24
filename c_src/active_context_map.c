//
// Copyright (c) 2025, Byteplug LLC.
//
// This source file is part of a project made by the Erlangsters community and
// is released under the MIT license. Please refer to the LICENSE.md file that
// can be found at the root of the project repository.
//
// Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
//
#include "active_context_map.h"
#include <string.h>
#include <assert.h>

void active_context_map_init(ActiveContextMap* map) {
    assert(map != NULL);
    memset(map, 0, sizeof(ActiveContextMap));
}

bool active_context_map_add(ActiveContextMap* map, const ErlNifPid* pid, EGLContext context) {
    if (map == NULL || pid == NULL || map->count >= MAX_ACTIVE_CONTEXTS) {
        return false;
    }

    // Check if PID or context already exists
    for (size_t i = 0; i < map->count; i++) {
        if (enif_compare_pids(pid, &map->entries[i].pid) == 0 ||
            map->entries[i].context == context) {
            return false;
        }
    }

    // Add new entry
    map->entries[map->count].pid = *pid;
    map->entries[map->count].context = context;
    map->count++;
    return true;
}

bool active_context_map_remove_by_pid(ActiveContextMap* map, const ErlNifPid* pid) {
    if (map == NULL || pid == NULL) {
        return false;
    }

    for (size_t i = 0; i < map->count; i++) {
        if (enif_compare_pids(pid, &map->entries[i].pid) == 0) {
            // Move last element to this position (if not last)
            if (i < map->count - 1) {
                map->entries[i] = map->entries[map->count - 1];
            }
            map->count--;
            return true;
        }
    }
    return false;
}

bool active_context_map_remove_by_context(ActiveContextMap* map, EGLContext context) {
    if (map == NULL) {
        return false;
    }

    for (size_t i = 0; i < map->count; i++) {
        if (map->entries[i].context == context) {
            // Move last element to this position (if not last)
            if (i < map->count - 1) {
                map->entries[i] = map->entries[map->count - 1];
            }
            map->count--;
            return true;
        }
    }
    return false;
}

size_t active_context_map_size(const ActiveContextMap* map) {
    if (map == NULL) {
        return 0;
    }
    return map->count;
}

EGLContext* active_context_map_find_by_pid(ActiveContextMap* map, const ErlNifPid* pid) {
    if (map == NULL || pid == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < map->count; i++) {
        if (enif_compare_pids(pid, &map->entries[i].pid) == 0) {
            return &map->entries[i].context;
        }
    }
    return NULL;
}

ErlNifPid* active_context_map_find_by_context(ActiveContextMap* map, EGLContext context) {
    if (map == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < map->count; i++) {
        if (map->entries[i].context == context) {
            return &map->entries[i].pid;
        }
    }
    return NULL;
}
