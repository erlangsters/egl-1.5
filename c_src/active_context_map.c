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

bool active_context_map_add(
    ActiveContextMap* map,
    const ErlNifPid* pid,
    EGLDisplay display,
    EGLSurface draw,
    EGLSurface read,
    EGLContext context
) {
    if (map == NULL || pid == NULL || map->count >= MAX_ACTIVE_CONTEXTS) {
        return false;
    }

    for (size_t i = 0; i < map->count; i++) {
        if (enif_compare_pids(pid, &map->entries[i].pid) == 0 ||
            map->entries[i].context == context) {
            return false;
        }
    }

    map->entries[map->count].pid = *pid;
    map->entries[map->count].display = display;
    map->entries[map->count].draw = draw;
    map->entries[map->count].read = read;
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

void active_context_map_remove_by_surface(ActiveContextMap* map, EGLSurface surface)
{
    if (map == NULL) {
        return;
    }

    size_t i = 0;
    while (i < map->count) {
        if (map->entries[i].draw == surface || map->entries[i].read == surface) {
            if (i < map->count - 1) {
                map->entries[i] = map->entries[map->count - 1];
            }
            map->count--;
        } else {
            i++;
        }
    }
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

PidContextEntry* active_context_map_entry_by_pid(ActiveContextMap* map, const ErlNifPid* pid) {
    if (map == NULL || pid == NULL) {
        return NULL;
    }

    for (size_t i = 0; i < map->count; i++) {
        if (enif_compare_pids(pid, &map->entries[i].pid) == 0) {
            return &map->entries[i];
        }
    }
    return NULL;
}

EGLContext* active_context_map_find_by_pid(ActiveContextMap* map, const ErlNifPid* pid) {
    PidContextEntry* entry = active_context_map_entry_by_pid(map, pid);
    if (entry == NULL) {
        return NULL;
    }
    return &entry->context;
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
