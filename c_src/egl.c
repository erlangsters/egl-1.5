#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <erl_nif.h>
#include <EGL/egl.h>
#include <EGL/eglext.h>
#include "command_executor.h"
#include "context_map.h"
#include "active_context_map.h"

#ifndef EGL_PLATFORM_WAYLAND_KHR
#define EGL_PLATFORM_WAYLAND_KHR 0x31D8
#endif
#ifndef EGL_PLATFORM_X11_KHR
#define EGL_PLATFORM_X11_KHR 0x31D5
#endif
#ifndef EGL_PLATFORM_ANGLE_ANGLE
#define EGL_PLATFORM_ANGLE_ANGLE 0x3202
#endif

#if defined(_WIN32)
#define EXPORT_SYMBOL __declspec(dllexport)
#else
#define EXPORT_SYMBOL
#endif

static ErlNifResourceType* egl_display_resource_type = NULL;
static ErlNifResourceType* egl_config_resource_type = NULL;
static ErlNifResourceType* egl_surface_resource_type = NULL;
static ErlNifResourceType* egl_context_resource_type = NULL;
static ErlNifResourceType* egl_client_buffer_resource_type = NULL;
static ErlNifResourceType* egl_sync_resource_type = NULL;
static ErlNifResourceType* egl_image_resource_type = NULL;

ERL_NIF_TERM undefined_atom;
ERL_NIF_TERM ok_atom;
ERL_NIF_TERM not_ok_atom;
ERL_NIF_TERM true_atom;
ERL_NIF_TERM false_atom;

ERL_NIF_TERM egl_no_display_atom;
ERL_NIF_TERM egl_no_surface_atom;
ERL_NIF_TERM egl_no_context_atom;

ERL_NIF_TERM egl_success_atom;
ERL_NIF_TERM egl_not_initialized_atom;
ERL_NIF_TERM egl_bad_access_atom;
ERL_NIF_TERM egl_bad_alloc_atom;
ERL_NIF_TERM egl_bad_attribute_atom;
ERL_NIF_TERM egl_bad_context_atom;
ERL_NIF_TERM egl_bad_config_atom;
ERL_NIF_TERM egl_bad_current_surface_atom;
ERL_NIF_TERM egl_bad_display_atom;
ERL_NIF_TERM egl_bad_surface_atom;
ERL_NIF_TERM egl_bad_match_atom;
ERL_NIF_TERM egl_bad_parameter_atom;
ERL_NIF_TERM egl_bad_native_pixmap_atom;
ERL_NIF_TERM egl_bad_native_window_atom;
ERL_NIF_TERM egl_context_lost_atom;

ERL_NIF_TERM egl_client_apis_atom;
ERL_NIF_TERM egl_vendor_atom;
ERL_NIF_TERM egl_version_atom;
ERL_NIF_TERM egl_extensions_atom;

ERL_NIF_TERM egl_core_native_engine_atom;

ERL_NIF_TERM default_display_atom;
ERL_NIF_TERM wayland_atom;
ERL_NIF_TERM x11_atom;
ERL_NIF_TERM angle_atom;

static ContextMap* context_map = NULL;
static ActiveContextMap active_context_map;

typedef struct EglDisplayResource {
    EGLDisplay display;
    int alive;
    struct EglDisplayResource* intern_next;
} EglDisplayResource;

typedef struct EglConfigResource {
    EGLDisplay display;
    EGLConfig config;
    int alive;
    struct EglConfigResource* intern_next;
} EglConfigResource;

typedef struct EglSurfaceResource {
    EGLDisplay display;
    EGLSurface surface;
    int alive;
    struct EglSurfaceResource* intern_next;
} EglSurfaceResource;

typedef struct EglContextResource {
    EGLDisplay display;
    EGLContext context;
    int alive;
    struct EglContextResource* intern_next;
} EglContextResource;

static EglDisplayResource* interned_displays = NULL;
static EglConfigResource* interned_configs = NULL;
static EglSurfaceResource* interned_surfaces = NULL;
static EglContextResource* interned_contexts = NULL;

extern EXPORT_SYMBOL ErlNifResourceType* get_egl_window_resource_type(ErlNifEnv* env) {
    static ErlNifResourceType* egl_window_resource_type = NULL;
    if (!egl_window_resource_type) {
        egl_window_resource_type = enif_open_resource_type(env, NULL, "egl_window", NULL, ERL_NIF_RT_CREATE, NULL);

        if (egl_window_resource_type == NULL) {
            fprintf(stderr, "failed to open 'EGL window' resource type\n");
            return NULL;
        }
    }
    return egl_window_resource_type;
}

extern EXPORT_SYMBOL ErlNifResourceType* get_egl_native_display_resource_type(ErlNifEnv* env) {
    static ErlNifResourceType* egl_native_display_resource_type = NULL;
    if (!egl_native_display_resource_type) {
        egl_native_display_resource_type = enif_open_resource_type(
            env, NULL, "egl_native_display", NULL, ERL_NIF_RT_CREATE, NULL);

        if (egl_native_display_resource_type == NULL) {
            fprintf(stderr, "failed to open 'EGL native display' resource type\n");
            return NULL;
        }
    }
    return egl_native_display_resource_type;
}

extern EXPORT_SYMBOL ERL_NIF_TERM egl_execute_command(
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[]
) {
    // XXX: The caller should be able to know if the command was executed.
    //      Perhaps add some variations of this function to fallback on
    //      executing the command in the current thread.

    // We must execute a NIF function (most of the time, those are OpenGL
    // commands) in the OS thread that is associated (by a bound OpenGL
    // context) to the BEAM process.
    ErlNifPid pid;
    enif_self(env, &pid);

    EGLContext* context = active_context_map_find_by_pid(&active_context_map, &pid);
    if (context != NULL) {
        // The BEAM process has an active context, we can execute the command
        // on a separate OS thread.
        CommandExecutor* command_executor = context_map_get(context_map, *context);
        assert(command_executor != NULL);

        ERL_NIF_TERM result;
        command_executor_execute(
            command_executor,
            function,
            env,
            argc,
            argv,
            &result
        );
        return result;
    }
    else {
        // XXX: Return 'undefine' atom instead ?
        return enif_make_badarg(env);
    }
}

static void beam_unlink_display(EglDisplayResource* resource)
{
    EglDisplayResource** slot = &interned_displays;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->intern_next;
            resource->intern_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->intern_next;
    }
}

static void beam_unlink_config(EglConfigResource* resource)
{
    EglConfigResource** slot = &interned_configs;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->intern_next;
            resource->intern_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->intern_next;
    }
}

static void beam_unlink_surface(EglSurfaceResource* resource)
{
    EglSurfaceResource** slot = &interned_surfaces;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->intern_next;
            resource->intern_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->intern_next;
    }
}

static void beam_unlink_context(EglContextResource* resource)
{
    EglContextResource** slot = &interned_contexts;
    while (*slot != NULL) {
        if (*slot == resource) {
            *slot = resource->intern_next;
            resource->intern_next = NULL;
            enif_release_resource(resource);
            return;
        }
        slot = &(*slot)->intern_next;
    }
}

static void beam_poison_display(EglDisplayResource* resource)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
    resource->display = EGL_NO_DISPLAY;
    beam_unlink_display(resource);
}

static void beam_poison_config(EglConfigResource* resource)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
    resource->config = NULL;
    resource->display = EGL_NO_DISPLAY;
    beam_unlink_config(resource);
}

static void beam_poison_surface(EglSurfaceResource* resource)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
    resource->surface = EGL_NO_SURFACE;
    resource->display = EGL_NO_DISPLAY;
    beam_unlink_surface(resource);
}

static void beam_poison_context(EglContextResource* resource)
{
    if (!resource->alive) {
        return;
    }
    resource->alive = 0;
    resource->context = EGL_NO_CONTEXT;
    resource->display = EGL_NO_DISPLAY;
    beam_unlink_context(resource);
}

static ERL_NIF_TERM beam_intern_display(ErlNifEnv* env, EGLDisplay display)
{
    EglDisplayResource* it;
    for (it = interned_displays; it != NULL; it = it->intern_next) {
        if (it->alive && it->display == display) {
            return enif_make_resource(env, it);
        }
    }

    EglDisplayResource* resource = enif_alloc_resource(
        egl_display_resource_type, sizeof(EglDisplayResource));
    resource->display = display;
    resource->alive = 1;
    resource->intern_next = interned_displays;
    interned_displays = resource;
    enif_keep_resource(resource);

    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static ERL_NIF_TERM beam_intern_config(ErlNifEnv* env, EGLDisplay display, EGLConfig config)
{
    EglConfigResource* it;
    for (it = interned_configs; it != NULL; it = it->intern_next) {
        if (it->alive && it->config == config) {
            return enif_make_resource(env, it);
        }
    }

    EglConfigResource* resource = enif_alloc_resource(
        egl_config_resource_type, sizeof(EglConfigResource));
    resource->display = display;
    resource->config = config;
    resource->alive = 1;
    resource->intern_next = interned_configs;
    interned_configs = resource;
    enif_keep_resource(resource);

    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static ERL_NIF_TERM beam_intern_surface(ErlNifEnv* env, EGLDisplay display, EGLSurface surface)
{
    EglSurfaceResource* it;
    for (it = interned_surfaces; it != NULL; it = it->intern_next) {
        if (it->alive && it->surface == surface) {
            return enif_make_resource(env, it);
        }
    }

    EglSurfaceResource* resource = enif_alloc_resource(
        egl_surface_resource_type, sizeof(EglSurfaceResource));
    resource->display = display;
    resource->surface = surface;
    resource->alive = 1;
    resource->intern_next = interned_surfaces;
    interned_surfaces = resource;
    enif_keep_resource(resource);

    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static ERL_NIF_TERM beam_intern_context(ErlNifEnv* env, EGLDisplay display, EGLContext context)
{
    EglContextResource* it;
    for (it = interned_contexts; it != NULL; it = it->intern_next) {
        if (it->alive && it->context == context) {
            return enif_make_resource(env, it);
        }
    }

    EglContextResource* resource = enif_alloc_resource(
        egl_context_resource_type, sizeof(EglContextResource));
    resource->display = display;
    resource->context = context;
    resource->alive = 1;
    resource->intern_next = interned_contexts;
    interned_contexts = resource;
    enif_keep_resource(resource);

    ERL_NIF_TERM term = enif_make_resource(env, resource);
    enif_release_resource(resource);
    return term;
}

static int beam_get_display(ErlNifEnv* env, ERL_NIF_TERM term, EglDisplayResource** out)
{
    EglDisplayResource* resource;
    if (!enif_get_resource(env, term, egl_display_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->display == EGL_NO_DISPLAY) {
        return 0;
    }
    *out = resource;
    return 1;
}

static int beam_get_config(ErlNifEnv* env, ERL_NIF_TERM term, EglConfigResource** out)
{
    EglConfigResource* resource;
    if (!enif_get_resource(env, term, egl_config_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->config == NULL) {
        return 0;
    }
    *out = resource;
    return 1;
}

static int beam_get_surface(ErlNifEnv* env, ERL_NIF_TERM term, EglSurfaceResource** out)
{
    EglSurfaceResource* resource;
    if (!enif_get_resource(env, term, egl_surface_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->surface == EGL_NO_SURFACE) {
        return 0;
    }
    *out = resource;
    return 1;
}

static int beam_get_context(ErlNifEnv* env, ERL_NIF_TERM term, EglContextResource** out)
{
    EglContextResource* resource;
    if (!enif_get_resource(env, term, egl_context_resource_type, (void**)&resource)) {
        return 0;
    }
    if (!resource->alive || resource->context == EGL_NO_CONTEXT) {
        return 0;
    }
    *out = resource;
    return 1;
}

static void beam_poison_display_family(EGLDisplay display, int destroy_executors)
{
    EglContextResource* context_it = interned_contexts;
    while (context_it != NULL) {
        EglContextResource* next = context_it->intern_next;
        if (context_it->alive && context_it->display == display) {
            if (destroy_executors && context_it->context != EGL_NO_CONTEXT) {
                CommandExecutor* command_executor =
                    context_map_get(context_map, context_it->context);
                if (command_executor != NULL) {
                    command_executor_destroy(command_executor);
                    context_map_erase(context_map, context_it->context);
                }
                active_context_map_remove_by_context(&active_context_map, context_it->context);
            }
            beam_poison_context(context_it);
        }
        context_it = next;
    }

    EglSurfaceResource* surface_it = interned_surfaces;
    while (surface_it != NULL) {
        EglSurfaceResource* next = surface_it->intern_next;
        if (surface_it->alive && surface_it->display == display) {
            beam_poison_surface(surface_it);
        }
        surface_it = next;
    }

    EglConfigResource* config_it = interned_configs;
    while (config_it != NULL) {
        EglConfigResource* next = config_it->intern_next;
        if (config_it->alive && config_it->display == display) {
            beam_poison_config(config_it);
        }
        config_it = next;
    }

    EglDisplayResource* display_it = interned_displays;
    while (display_it != NULL) {
        EglDisplayResource* next = display_it->intern_next;
        if (display_it->alive && display_it->display == display) {
            beam_poison_display(display_it);
        }
        display_it = next;
    }
}

static void egl_display_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    EglDisplayResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
        resource->display = EGL_NO_DISPLAY;
        beam_unlink_display(resource);
    }
}

static void egl_config_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    EglConfigResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
        resource->config = NULL;
        resource->display = EGL_NO_DISPLAY;
        beam_unlink_config(resource);
    }
}

static void egl_surface_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    EglSurfaceResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
        resource->surface = EGL_NO_SURFACE;
        resource->display = EGL_NO_DISPLAY;
        beam_unlink_surface(resource);
    }
}

static void egl_context_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    EglContextResource* resource = obj;
    if (resource->alive) {
        resource->alive = 0;
        resource->context = EGL_NO_CONTEXT;
        resource->display = EGL_NO_DISPLAY;
        beam_unlink_context(resource);
    }
}

static void egl_client_buffer_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    (void)obj;
}

static void egl_sync_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    (void)obj;
}

static void egl_image_resource_dtor(ErlNifEnv* env, void* obj) {
    (void)env;
    (void)obj;
}

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
    (void)priv_data;
    (void)arg;

    // The first call initializes the EGL window resource type which we need
    // to do here.
    ErlNifResourceType* egl_window_resource_type = get_egl_window_resource_type(env);
    if (egl_window_resource_type == NULL) {
        fprintf(stderr, "failed to get 'EGL window' resource type\n");
        return -1;
    }

    ErlNifResourceType* egl_native_display_resource_type =
        get_egl_native_display_resource_type(env);
    if (egl_native_display_resource_type == NULL) {
        fprintf(stderr, "failed to get 'EGL native display' resource type\n");
        return -1;
    }

    egl_display_resource_type = enif_open_resource_type(env, NULL, "egl_display", egl_display_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_display_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL display' resource type\n");
        return -1;
    }

    egl_config_resource_type = enif_open_resource_type(env, NULL, "egl_config", egl_config_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_config_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL config' resource type\n");
        return -1;
    }

    egl_surface_resource_type = enif_open_resource_type(env, NULL, "egl_surface", egl_surface_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_surface_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL surface' resource type\n");
        return -1;
    }

    egl_context_resource_type = enif_open_resource_type(env, NULL, "egl_context", egl_context_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_context_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL context' resource type\n");
        return -1;
    }

    egl_client_buffer_resource_type = enif_open_resource_type(env, NULL, "egl_client_buffer", egl_client_buffer_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_client_buffer_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL client buffer' resource type\n");
        return -1;
    }

    egl_sync_resource_type = enif_open_resource_type(env, NULL, "egl_sync", egl_sync_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_sync_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL sync' resource type\n");
        return -1;
    }

    egl_image_resource_type = enif_open_resource_type(env, NULL, "egl_image", egl_image_resource_dtor, ERL_NIF_RT_CREATE, NULL);
    if (egl_image_resource_type == NULL) {
        fprintf(stderr, "failed to open 'EGL image' resource type\n");
        return -1;
    }

    undefined_atom = enif_make_atom(env, "undefined");
    ok_atom = enif_make_atom(env, "ok");
    not_ok_atom = enif_make_atom(env, "not_ok");
    true_atom = enif_make_atom(env, "true");
    false_atom = enif_make_atom(env, "false");

    egl_no_display_atom = enif_make_atom(env, "no_display");
    egl_no_surface_atom = enif_make_atom(env, "no_surface");
    egl_no_context_atom = enif_make_atom(env, "no_context");

    egl_success_atom = enif_make_atom(env, "success");
    egl_not_initialized_atom = enif_make_atom(env, "not_initialized");
    egl_bad_access_atom = enif_make_atom(env, "bad_access");
    egl_bad_alloc_atom = enif_make_atom(env, "bad_alloc");
    egl_bad_attribute_atom = enif_make_atom(env, "bad_attribute");
    egl_bad_context_atom = enif_make_atom(env, "bad_context");
    egl_bad_config_atom = enif_make_atom(env, "bad_config");
    egl_bad_current_surface_atom = enif_make_atom(env, "bad_current_surface");
    egl_bad_display_atom = enif_make_atom(env, "bad_display");
    egl_bad_surface_atom = enif_make_atom(env, "bad_surface");
    egl_bad_match_atom = enif_make_atom(env, "bad_match");
    egl_bad_parameter_atom = enif_make_atom(env, "bad_parameter");
    egl_bad_native_pixmap_atom = enif_make_atom(env, "bad_native_pixmap");
    egl_bad_native_window_atom = enif_make_atom(env, "bad_native_window");
    egl_context_lost_atom = enif_make_atom(env, "context_lost");

    egl_client_apis_atom = enif_make_atom(env, "client_apis");
    egl_vendor_atom = enif_make_atom(env, "vendor");
    egl_version_atom = enif_make_atom(env, "version");
    egl_extensions_atom = enif_make_atom(env, "extensions");

    egl_core_native_engine_atom = enif_make_atom(env, "core_native_engine");

    default_display_atom = enif_make_atom(env, "default_display");
    wayland_atom = enif_make_atom(env, "wayland");
    x11_atom = enif_make_atom(env, "x11");
    angle_atom = enif_make_atom(env, "angle");

    context_map = context_map_create(4);
    if (context_map == NULL) {
        fprintf(stderr, "failed to create OpenGL context map\n");
        return -1;
    }
    active_context_map_init(&active_context_map);

    return 0;
}

static void nif_module_unload(ErlNifEnv* caller_env, void* priv_data)
{
    (void)caller_env;
    (void)priv_data;

    if (context_map) {
        // xxx
        context_map_destroy(context_map);
        context_map = NULL;
    }
}

static ERL_NIF_TERM nif_choose_config(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // Second argument is a list of integers that was prepared on the Erlang
    // side so we just pass it as is to eglChooseConfig. We just have to
    // convert it into a C array and append EGL_NONE to it.

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    ERL_NIF_TERM list = argv[1];
    unsigned int list_length;
    if (!enif_get_list_length(env, list, &list_length)) {
        return enif_make_badarg(env);
    }

    EGLint* attrib_list = (EGLint*)malloc((list_length + 1) * sizeof(EGLint));
    if (attrib_list == NULL) {
        // XXX: Not sure what to do in this scenario.
        return enif_make_atom(env, "error_alloc");
    }

    ERL_NIF_TERM head, tail;
    int value;
    unsigned int i = 0;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        if (!enif_get_int(env, head, &value)) {
            free(attrib_list);
            return enif_make_badarg(env);
        }
        attrib_list[i++] = (EGLint)value;
        list = tail;
    }
    attrib_list[i] = EGL_NONE;

    EGLint num_configs;
    EGLBoolean result = eglChooseConfig(display, attrib_list, NULL, 0, &num_configs);
    if (result == EGL_FALSE) {
        return not_ok_atom;
    }
    else {
        EGLConfig* configs = (EGLConfig*)malloc(num_configs * sizeof(EGLConfig));
        if (configs == NULL) {
            // XXX: What should we do here ?
            return enif_make_atom(env, "error_alloc");
        }
        result = eglChooseConfig(display, attrib_list, configs, num_configs, &num_configs);

        if (result == EGL_FALSE) {
            free(attrib_list);
            free(configs);
            return not_ok_atom;
        }
        else {
            ERL_NIF_TERM config_list = enif_make_list(env, 0);
            for (EGLint i = 0; i < num_configs; i++) {
                ERL_NIF_TERM config_term = beam_intern_config(env, display, configs[i]);
                config_list = enif_make_list_cell(env, config_term, config_list);
            }

            free(attrib_list);
            free(configs);

            return enif_make_tuple2(
                env,
                enif_make_atom(env, "ok"),
                config_list
            );
        }
    }
}

#if 0
static ERL_NIF_TERM nif_copy_buffers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglCopyBuffers (EGLDisplay dpy, EGLSurface surface, EGLNativePixmapType target);

    return enif_make_atom(env, "ok");
}
#endif

static ERL_NIF_TERM nif_create_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // Third argument is a list of integers that was prepared on the Erlang
    // side so we just pass it as is to eglCreateContext. We just have
    // to convert it into a C array and append EGL_NONE to it.

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglConfigResource* config_resource;
    if (!beam_get_config(env, argv[1], &config_resource)) {
        return enif_make_badarg(env);
    }
    EGLConfig config = config_resource->config;

    EGLContext share_context;
    if (enif_is_identical(argv[2], enif_make_atom(env, "no_context"))) {
        share_context = EGL_NO_CONTEXT;
    }
    else {
        EglContextResource* share_context_resource;
        if (!beam_get_context(env, argv[2], &share_context_resource)) {
            return enif_make_badarg(env);
        }
        share_context = share_context_resource->context;
    }

    ERL_NIF_TERM list = argv[3];
    unsigned int list_length;
    if (!enif_get_list_length(env, list, &list_length)) {
        return enif_make_badarg(env);
    }

    EGLint* attrib_list = (EGLint*)malloc((list_length + 1) * sizeof(EGLint));
    if (attrib_list == NULL) {
        // XXX: Not sure what to do in this scenario.
        return enif_make_atom(env, "error_alloc");
    }

    ERL_NIF_TERM head, tail;
    int value;
    unsigned int i = 0;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        if (!enif_get_int(env, head, &value)) {
            free(attrib_list);
            return enif_make_badarg(env);
        }
        attrib_list[i++] = (EGLint)value;
        list = tail;
    }
    attrib_list[i] = EGL_NONE;

    EGLContext result = eglCreateContext(display, config, share_context, attrib_list);
    if (result == EGL_NO_CONTEXT) {
        return not_ok_atom;
    }
    else {
        CommandExecutor* command_executor = context_map_put(context_map, result);
        command_executor_init(command_executor);

        return enif_make_tuple2(
            env,
            enif_make_atom(env, "ok"),
            beam_intern_context(env, display, result)
        );
    }
}

static ERL_NIF_TERM nif_create_pbuffer_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    // Second argument is a list of integers that was prepared on the Erlang
    // side so we just pass it as is to eglCreatePbufferSurface. We just have
    // to convert it into a C array and append EGL_NONE to it.
    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglConfigResource* config_resource;
    if (!beam_get_config(env, argv[1], &config_resource)) {
        return enif_make_badarg(env);
    }
    EGLConfig config = config_resource->config;

    ERL_NIF_TERM list = argv[2];
    unsigned int list_length;
    if (!enif_get_list_length(env, list, &list_length)) {
        return enif_make_badarg(env);
    }

    EGLint* attrib_list = (EGLint*)malloc((list_length + 1) * sizeof(EGLint));
    if (attrib_list == NULL) {
        // XXX: Not sure what to do in this scenario.
        return enif_make_atom(env, "error_alloc");
    }

    ERL_NIF_TERM head, tail;
    int value;
    unsigned int i = 0;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        if (!enif_get_int(env, head, &value)) {
            free(attrib_list);
            return enif_make_badarg(env);
        }
        attrib_list[i++] = (EGLint)value;
        list = tail;
    }
    attrib_list[i] = EGL_NONE;

    EGLSurface result = eglCreatePbufferSurface(display, config, attrib_list);
    if (result == EGL_NO_SURFACE) {
        return not_ok_atom;
    }
    else {
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "ok"),
            beam_intern_surface(env, display, result)
        );
    }
}

#if 0
static ERL_NIF_TERM nif_create_pixmap_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePixmapSurface (EGLDisplay dpy, EGLConfig config, EGLNativePixmapType pixmap, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}
#endif

static ERL_NIF_TERM nif_create_window_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglConfigResource* config_resource;
    if (!beam_get_config(env, argv[1], &config_resource)) {
        return enif_make_badarg(env);
    }
    EGLConfig config = config_resource->config;

    ErlNifResourceType* egl_window_resource_type = get_egl_window_resource_type(env);
    void* native_window_resource;
    if (!enif_get_resource(env, argv[2], egl_window_resource_type, &native_window_resource)) {
        return enif_make_badarg(env);
    }
    EGLNativeWindowType native_window = *((EGLNativeWindowType*)native_window_resource);

    ERL_NIF_TERM list = argv[3];
    unsigned int list_length;
    if (!enif_get_list_length(env, list, &list_length)) {
        return enif_make_badarg(env);
    }

    EGLint* attrib_list = (EGLint*)malloc((list_length + 1) * sizeof(EGLint));
    if (attrib_list == NULL) {
        return enif_make_atom(env, "error_alloc");
    }

    ERL_NIF_TERM head, tail;
    int value;
    unsigned int i = 0;
    while (enif_get_list_cell(env, list, &head, &tail)) {
        if (!enif_get_int(env, head, &value)) {
            free(attrib_list);
            return enif_make_badarg(env);
        }
        attrib_list[i++] = (EGLint)value;
        list = tail;
    }
    attrib_list[i] = EGL_NONE;

    const EGLint* attribs = (list_length == 0) ? NULL : attrib_list;
    EGLSurface result = eglCreateWindowSurface(display, config, native_window, attribs);
    free(attrib_list);
    if (result == EGL_NO_SURFACE) {
        return not_ok_atom;
    }
    else {
        return enif_make_tuple2(
            env,
            enif_make_atom(env, "ok"),
            beam_intern_surface(env, display, result)
        );
    }
}

static ERL_NIF_TERM nif_destroy_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglContextResource* context_resource;
    if (!beam_get_context(env, argv[1], &context_resource)) {
        return enif_make_badarg(env);
    }
    EGLContext context = context_resource->context;

    EGLBoolean result = eglDestroyContext(display, context);

    if (result == EGL_TRUE) {

        CommandExecutor* command_executor = context_map_get(context_map, context);
        assert(command_executor != NULL);
        command_executor_destroy(command_executor);
        bool success = context_map_erase(context_map, context);
        assert(success);
        active_context_map_remove_by_context(&active_context_map, context);
        beam_poison_context(context_resource);

        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_destroy_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglSurfaceResource* surface_resource;
    if (!beam_get_surface(env, argv[1], &surface_resource)) {
        return enif_make_badarg(env);
    }
    EGLSurface surface = surface_resource->surface;

    EGLBoolean result = eglDestroySurface(display, surface);
    if (result == EGL_TRUE) {
        active_context_map_remove_by_surface(&active_context_map, surface);
        beam_poison_surface(surface_resource);
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_get_config_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglConfigResource* config_resource;
    if (!beam_get_config(env, argv[1], &config_resource)) {
        return enif_make_badarg(env);
    }
    EGLConfig config = config_resource->config;

    EGLint attribute;
    if (!enif_get_int(env, argv[2], &attribute)) {
        return enif_make_badarg(env);
    }

    EGLint value;
    EGLBoolean result = eglGetConfigAttrib(display, config, attribute, &value);
    if (result == EGL_FALSE) {
        return not_ok_atom;
    }
    else {
        return enif_make_tuple2(
            env,
            ok_atom,
            enif_make_int(env, value)
        );
    }

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_configs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EGLint num_configs;
    EGLBoolean result = eglGetConfigs(display, NULL, 0, &num_configs);
    if (result == EGL_FALSE) {
        return not_ok_atom;
    }
    else {
        EGLConfig* configs = (EGLConfig*)malloc(num_configs * sizeof(EGLConfig));
        if (configs == NULL) {
            // XXX: What should we do here ?
            return enif_make_atom(env, "error_alloc");
        }
        result = eglGetConfigs(display, configs, num_configs, &num_configs);

        if (result == EGL_FALSE) {
            free(configs);
            return not_ok_atom;
        }
        else {
            ERL_NIF_TERM config_list = enif_make_list(env, 0);
            for (EGLint i = 0; i < num_configs; i++) {
                ERL_NIF_TERM config_term = beam_intern_config(env, display, configs[i]);
                config_list = enif_make_list_cell(env, config_term, config_list);
            }

            free(configs);

            return enif_make_tuple2(
                env,
                enif_make_atom(env, "ok"),
                config_list
            );
        }
    }
}

static ERL_NIF_TERM nif_get_current_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    ErlNifPid pid;
    enif_self(env, &pid);
    PidContextEntry* entry = active_context_map_entry_by_pid(&active_context_map, &pid);
    if (entry == NULL || entry->display == EGL_NO_DISPLAY) {
        return egl_no_display_atom;
    }
    return beam_intern_display(env, entry->display);
}

static ERL_NIF_TERM nif_get_current_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EGLint readdraw;
    if (!enif_get_int(env, argv[0], &readdraw)) {
        return enif_make_badarg(env);
    }

    ErlNifPid pid;
    enif_self(env, &pid);
    PidContextEntry* entry = active_context_map_entry_by_pid(&active_context_map, &pid);
    if (entry == NULL) {
        return egl_no_surface_atom;
    }

    EGLSurface surface;
    if (readdraw == EGL_DRAW) {
        surface = entry->draw;
    } else if (readdraw == EGL_READ) {
        surface = entry->read;
    } else {
        return enif_make_badarg(env);
    }

    if (surface == EGL_NO_SURFACE) {
        return egl_no_surface_atom;
    }
    return beam_intern_surface(env, entry->display, surface);
}

static ERL_NIF_TERM nif_get_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    if (!enif_is_identical(argv[0], default_display_atom)) {
        return enif_make_badarg(env);
    }

    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);

    if (display == EGL_NO_DISPLAY) {
        return enif_make_atom(env, "no_display");
    }
    return beam_intern_display(env, display);
}

static ERL_NIF_TERM nif_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    EGLint error = eglGetError();
    switch (error) {
        case EGL_SUCCESS:
            return egl_success_atom;
        case EGL_NOT_INITIALIZED:
            return egl_not_initialized_atom;
        case EGL_BAD_ACCESS:
            return egl_bad_access_atom;
        case EGL_BAD_ALLOC:
            return egl_bad_alloc_atom;
        case EGL_BAD_ATTRIBUTE:
            return egl_bad_attribute_atom;
        case EGL_BAD_CONTEXT:
            return egl_bad_context_atom;
        case EGL_BAD_CONFIG:
            return egl_bad_config_atom;
        case EGL_BAD_CURRENT_SURFACE:
            return egl_bad_current_surface_atom;
        case EGL_BAD_DISPLAY:
            return egl_bad_display_atom;
        case EGL_BAD_SURFACE:
            return egl_bad_surface_atom;
        case EGL_BAD_MATCH:
            return egl_bad_match_atom;
        case EGL_BAD_PARAMETER:
            return egl_bad_parameter_atom;
        case EGL_BAD_NATIVE_PIXMAP:
            return egl_bad_native_pixmap_atom;
        case EGL_BAD_NATIVE_WINDOW:
            return egl_bad_native_window_atom;
        case EGL_CONTEXT_LOST:
            return egl_context_lost_atom;
    }

    // XXX: Review this.
    return enif_make_atom(env, "undefined");
}

static ERL_NIF_TERM nif_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EGLint major, minor;
    EGLBoolean result = eglInitialize(display, &major, &minor);
    if (result == EGL_TRUE) {
        return enif_make_tuple2(
            env,
            ok_atom,
            enif_make_tuple2(env, enif_make_int(env, major), enif_make_int(env, minor))
        );
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM bind_context_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EGLSurface draw;
    if (enif_is_identical(argv[1], egl_no_surface_atom)) {
        draw = EGL_NO_SURFACE;
    } else {
        EglSurfaceResource* draw_resource;
        if (!beam_get_surface(env, argv[1], &draw_resource)) {
            return enif_make_badarg(env);
        }
        draw = draw_resource->surface;
    }

    EGLSurface read;
    if (enif_is_identical(argv[2], egl_no_surface_atom)) {
        read = EGL_NO_SURFACE;
    } else {
        EglSurfaceResource* read_resource;
        if (!beam_get_surface(env, argv[2], &read_resource)) {
            return enif_make_badarg(env);
        }
        read = read_resource->surface;
    }

    EglContextResource* context_resource;
    if (!beam_get_context(env, argv[3], &context_resource)) {
        return enif_make_badarg(env);
    }
    EGLContext context = context_resource->context;

    /* eglBindAPI is per-OS-thread. This function runs on the context
     * executor, so the client API must be bound here before make current.
     * bind_api/1 before make_current runs on a scheduler thread and does
     * not affect this one. After make_current, bind_api/1 follows the
     * executor. */
    EGLint client_type;
    if (eglQueryContext(display, context, EGL_CONTEXT_CLIENT_TYPE, &client_type) != EGL_TRUE) {
        return not_ok_atom;
    }
    if (eglBindAPI((EGLenum)client_type) != EGL_TRUE) {
        return not_ok_atom;
    }

    EGLBoolean result = eglMakeCurrent(display, draw, read, context);
    if (result == EGL_TRUE) {
        ErlNifPid pid;
        enif_self(env, &pid);

        ErlNifPid* bound_pid = active_context_map_find_by_context(&active_context_map, context);
        if (bound_pid != NULL) {
            bool success = active_context_map_remove_by_pid(&active_context_map, bound_pid);
            assert(success);
        }

        bool success = active_context_map_add(
            &active_context_map, &pid, display, draw, read, context);
        assert(success);

        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM unbind_context_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EGLSurface draw;
    if (enif_is_identical(argv[1], egl_no_surface_atom)) {
        draw = EGL_NO_SURFACE;
    } else {
        EglSurfaceResource* draw_resource;
        if (!beam_get_surface(env, argv[1], &draw_resource)) {
            return enif_make_badarg(env);
        }
        draw = draw_resource->surface;
    }

    EGLSurface read;
    if (enif_is_identical(argv[2], egl_no_surface_atom)) {
        read = EGL_NO_SURFACE;
    } else {
        EglSurfaceResource* read_resource;
        if (!beam_get_surface(env, argv[2], &read_resource)) {
            return enif_make_badarg(env);
        }
        read = read_resource->surface;
    }

    EGLBoolean result = eglMakeCurrent(display, draw, read, EGL_NO_CONTEXT);
    if (result == EGL_TRUE) {
        ErlNifPid pid;
        enif_self(env, &pid);
        active_context_map_remove_by_pid(&active_context_map, &pid);
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_make_current(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Note that the implementation of this function is a bit difficult to
    // understand because instead of working with one OS thread per BEAM
    // process, we work with one OS thread per EGL context.
    (void)argc;

    // We need to handle two requests: binding and unbinding a context.
    if (enif_is_identical(argv[3], egl_no_context_atom)) {
        ErlNifPid pid;
        enif_self(env, &pid);

        EGLContext* context = active_context_map_find_by_pid(&active_context_map, &pid);
        if (context != NULL) {
            CommandExecutor* command_executor = context_map_get(context_map, *context);
            assert(command_executor != NULL);

            ERL_NIF_TERM result;
            command_executor_execute(
                command_executor,
                unbind_context_nif,
                env,
                argc,
                argv,
                &result
            );
            return result;
        }
        return ok_atom;
    }
    else {
        // This is a "bind" request. We execute it on the OS thread associated
        // to the EGL context.
        // Note: we let the function update the active context map.
        EglContextResource* context_resource;
        if (!beam_get_context(env, argv[3], &context_resource)) {
            return enif_make_badarg(env);
        }
        EGLContext context = context_resource->context;

        CommandExecutor* command_executor = context_map_get(context_map, context);
        assert(command_executor != NULL);

        ERL_NIF_TERM result;
        command_executor_execute(
            command_executor,
            bind_context_nif,
            env,
            argc,
            argv,
            &result
        );
        return result;
    }
}

static ERL_NIF_TERM nif_query_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglContextResource* context_resource;
    if (!beam_get_context(env, argv[1], &context_resource)) {
        return enif_make_badarg(env);
    }
    EGLContext context = context_resource->context;

    EGLint attribute;
    if (!enif_get_int(env, argv[2], &attribute)) {
        return enif_make_badarg(env);
    }

    EGLint value;
    EGLBoolean result = eglQueryContext(display, context, attribute, &value);
    if (result == EGL_FALSE) {
        return not_ok_atom;
    }
    else {
        return enif_make_tuple2(
            env,
            ok_atom,
            enif_make_int(env, value)
        );
    }
}

static ERL_NIF_TERM nif_query_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EGLDisplay display;
    if (enif_is_identical(argv[0], egl_no_display_atom)) {
        display = EGL_NO_DISPLAY;
    }
    else {
        EglDisplayResource* display_resource;
        if (!beam_get_display(env, argv[0], &display_resource)) {
            return enif_make_badarg(env);
        }
        display = display_resource->display;
    }

    EGLint name;
    if (enif_is_identical(argv[1], egl_client_apis_atom)) {
        name = EGL_CLIENT_APIS;
    }
    else if (enif_is_identical(argv[1], egl_vendor_atom)) {
        name = EGL_VENDOR;
    }
    else if (enif_is_identical(argv[1], egl_version_atom)) {
        name = EGL_VERSION;
    }
    else if (enif_is_identical(argv[1], egl_extensions_atom)) {
        name = EGL_EXTENSIONS;
    }
    else {
        return enif_make_badarg(env);
    }

    const char* result = eglQueryString(display, name);
    if (result == NULL) {
        return not_ok_atom;
    }
    else {

        return enif_make_tuple2(
            env,
            ok_atom,
            enif_make_string(env, result, ERL_NIF_LATIN1)
        );
    }
}

static ERL_NIF_TERM nif_query_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglSurfaceResource* surface_resource;
    if (!beam_get_surface(env, argv[1], &surface_resource)) {
        return enif_make_badarg(env);
    }
    EGLSurface surface = surface_resource->surface;

    EGLint attribute;
    if (!enif_get_int(env, argv[2], &attribute)) {
        return enif_make_badarg(env);
    }

    EGLint value;
    EGLBoolean result = eglQuerySurface(display, surface, attribute, &value);
    if (result == EGL_FALSE) {
        return not_ok_atom;
    }
    else {
        return enif_make_tuple2(
            env,
            ok_atom,
            enif_make_int(env, value)
        );
    }
}

static ERL_NIF_TERM egl_swap_buffers_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglSurfaceResource* surface_resource;
    if (!beam_get_surface(env, argv[1], &surface_resource)) {
        return enif_make_badarg(env);
    }
    EGLSurface surface = surface_resource->surface;

    EGLBoolean result = eglSwapBuffers(display, surface);
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_swap_buffers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return egl_execute_command(
        egl_swap_buffers_nif,
        env,
        argc,
        argv
    );
}

static ERL_NIF_TERM nif_terminate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EGLBoolean result = eglTerminate(display);
    if (result == EGL_TRUE) {
        beam_poison_display_family(display, 1);
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_wait_gl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    EGLBoolean result = eglWaitGL();
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_wait_native(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    if (!enif_is_identical(argv[0], egl_core_native_engine_atom)) {
        return enif_make_badarg(env);
    }

    EGLBoolean result = eglWaitNative(EGL_CORE_NATIVE_ENGINE);
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

#if 0
static ERL_NIF_TERM nif_bind_tex_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglBindTexImage (EGLDisplay dpy, EGLSurface surface, EGLint buffer);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_release_tex_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglReleaseTexImage (EGLDisplay dpy, EGLSurface surface, EGLint buffer);

    return enif_make_atom(env, "ok");
}
#endif

static ERL_NIF_TERM nif_surface_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EglSurfaceResource* surface_resource;
    if (!beam_get_surface(env, argv[1], &surface_resource)) {
        return enif_make_badarg(env);
    }
    EGLSurface surface = surface_resource->surface;

    EGLint attribute;
    if (!enif_get_int(env, argv[2], &attribute)) {
        return enif_make_badarg(env);
    }

    EGLint value;
    if (!enif_get_int(env, argv[3], &value)) {
        return enif_make_badarg(env);
    }

    EGLBoolean result = eglSurfaceAttrib(display, surface, attribute, value);
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_swap_interval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EglDisplayResource* display_resource;
    if (!beam_get_display(env, argv[0], &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = display_resource->display;

    EGLint interval;
    if (!enif_get_int(env, argv[1], &interval)) {
        return enif_make_badarg(env);
    }

    EGLBoolean result = eglSwapInterval(display, interval);
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM bind_api_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    int api;
    if (!enif_get_int(env, argv[0], &api)) {
        return enif_make_badarg(env);
    }

    EGLBoolean result = eglBindAPI((EGLenum)api);
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_bind_api(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    ErlNifPid pid;
    enif_self(env, &pid);

    EGLContext* context = active_context_map_find_by_pid(&active_context_map, &pid);
    if (context != NULL) {
        return egl_execute_command(bind_api_nif, env, argc, argv);
    }
    return bind_api_nif(env, argc, argv);
}

static ERL_NIF_TERM nif_query_api(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    EGLenum result = eglQueryAPI();
    return enif_make_int(env, result);
}

#if 0
static ERL_NIF_TERM nif_create_pbuffer_from_client_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePbufferFromClientBuffer (EGLDisplay dpy, EGLenum buftype, EGLClientBuffer buffer, EGLConfig config, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}
#endif

static ERL_NIF_TERM nif_release_thread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    EGLBoolean result = eglReleaseThread();
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_wait_client(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)env;
    (void)argc;
    (void)argv;

    EGLBoolean result = eglWaitClient();
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
}

static ERL_NIF_TERM nif_get_current_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    ErlNifPid pid;
    enif_self(env, &pid);
    PidContextEntry* entry = active_context_map_entry_by_pid(&active_context_map, &pid);
    if (entry == NULL || entry->context == EGL_NO_CONTEXT) {
        return egl_no_context_atom;
    }
    return beam_intern_context(env, entry->display, entry->context);
}

#if 0
static ERL_NIF_TERM nif_create_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLSync EGLAPIENTRY eglCreateSync (EGLDisplay dpy, EGLenum type, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglDestroySync (EGLDisplay dpy, EGLSync sync);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_client_wait_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLint EGLAPIENTRY eglClientWaitSync (EGLDisplay dpy, EGLSync sync, EGLint flags, EGLTime timeout);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_sync_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglGetSyncAttrib (EGLDisplay dpy, EGLSync sync, EGLint attribute, EGLAttrib *value);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLImage EGLAPIENTRY eglCreateImage (EGLDisplay dpy, EGLContext ctx, EGLenum target, EGLClientBuffer buffer, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglDestroyImage (EGLDisplay dpy, EGLImage image);

    return enif_make_atom(env, "ok");
}
#endif

static ERL_NIF_TERM nif_get_platform_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;

    EGLenum platform;
    if (enif_is_identical(argv[0], wayland_atom)) {
        platform = EGL_PLATFORM_WAYLAND_KHR;
    } else if (enif_is_identical(argv[0], x11_atom)) {
        platform = EGL_PLATFORM_X11_KHR;
    } else if (enif_is_identical(argv[0], angle_atom)) {
        platform = EGL_PLATFORM_ANGLE_ANGLE;
    } else {
        return enif_make_badarg(env);
    }

    void* native_display;
    if (enif_is_identical(argv[1], default_display_atom)) {
        native_display = EGL_DEFAULT_DISPLAY;
    } else {
        ErlNifResourceType* native_display_resource_type =
            get_egl_native_display_resource_type(env);
        void* native_display_resource;
        if (!enif_get_resource(
                env,
                argv[1],
                native_display_resource_type,
                &native_display_resource
            )) {
            return enif_make_badarg(env);
        }
        native_display = *((void**)native_display_resource);
    }

    unsigned attrib_length;
    if (!enif_get_list_length(env, argv[2], &attrib_length) || attrib_length != 0) {
        return enif_make_badarg(env);
    }

    EGLDisplay display = eglGetPlatformDisplay(platform, native_display, NULL);
    if (display == EGL_NO_DISPLAY) {
        return egl_no_display_atom;
    }

    return beam_intern_display(env, display);
}

#if 0
static ERL_NIF_TERM nif_create_platform_window_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePlatformWindowSurface (EGLDisplay dpy, EGLConfig config, void *native_window, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_platform_pixmap_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePlatformPixmapSurface (EGLDisplay dpy, EGLConfig config, void *native_pixmap, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_wait_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void)argc;
    (void)argv;

    // EGLAPI EGLBoolean EGLAPIENTRY eglWaitSync (EGLDisplay dpy, EGLSync sync, EGLint flags);

    return enif_make_atom(env, "ok");
}
#endif

static ErlNifFunc nif_functions[] = {
    // EGL 1.0
    {"choose_config_raw", 2, nif_choose_config, 0},
    // {"copy_buffers", 3, nif_copy_buffers, 0},
    {"create_context_raw", 4, nif_create_context, 0},
    {"create_pbuffer_surface_raw", 3, nif_create_pbuffer_surface, 0},
    // {"create_pixmap_surface", 4, nif_create_pixmap_surface, 0},
    {"create_window_surface_raw", 4, nif_create_window_surface, 0},
    {"destroy_context", 2, nif_destroy_context, 0},
    {"destroy_surface", 2, nif_destroy_surface, 0},
    {"get_config_attrib_raw", 3, nif_get_config_attrib, 0},
    {"get_configs", 1, nif_get_configs, 0},
    {"get_current_display", 0, nif_get_current_display, 0},
    {"get_current_surface_raw", 1, nif_get_current_surface, 0},
    {"get_display", 1, nif_get_display, 0},
    {"get_error", 0, nif_get_error, 0},
    {"initialize", 1, nif_initialize, 0},
    {"make_current", 4, nif_make_current, 0},
    {"query_context_raw", 3, nif_query_context, 0},
    {"query_string", 2, nif_query_string, 0},
    {"query_surface_raw", 3, nif_query_surface, 0},
    {"swap_buffers", 2, nif_swap_buffers, 0},
    {"terminate", 1, nif_terminate, 0},
    {"wait_gl", 0, nif_wait_gl, 0},
    {"wait_native", 1, nif_wait_native, 0},
    // EGL 1.1
    // {"bind_tex_image", 3, nif_bind_tex_image, 0},
    // {"release_tex_image", 3, nif_release_tex_image, 0},
    {"surface_attrib_raw", 4, nif_surface_attrib, 0},
    {"swap_interval", 2, nif_swap_interval, 0},
    // EGL 1.2
    {"bind_api_raw", 1, nif_bind_api, 0},
    {"query_api_raw", 0, nif_query_api, 0},
    // {"create_pbuffer_from_client_buffer", 5, nif_create_pbuffer_from_client_buffer, 0},
    {"release_thread", 0, nif_release_thread, 0},
    {"wait_client", 0, nif_wait_client, 0},
    // EGL 1.4
    {"get_current_context", 0, nif_get_current_context, 0},
    // EGL 1.5
    // {"create_sync", 3, nif_create_sync, 0},
    // {"destroy_sync", 2, nif_destroy_sync, 0},
    // {"client_wait_sync", 4, nif_client_wait_sync, 0},
    // {"get_sync_attrib", 4, nif_get_sync_attrib, 0},
    // {"create_image", 5, nif_create_image, 0},
    // {"destroy_image", 2, nif_destroy_image, 0},
    {"get_platform_display", 3, nif_get_platform_display, 0},
    // {"create_platform_window_surface", 4, nif_create_platform_window_surface, 0},
    // {"create_platform_pixmap_surface", 4, nif_create_platform_pixmap_surface, 0},
    // {"wait_sync", 3, nif_wait_sync, 0}
};

ERL_NIF_INIT(
    egl,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
