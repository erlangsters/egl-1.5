#include <string.h>
#include <erl_nif.h>
#include <EGL/egl.h>

static ErlNifResourceType* egl_display_resource_type = NULL;
static ErlNifResourceType* egl_config_resource_type = NULL;
static ErlNifResourceType* egl_surface_resource_type = NULL;
static ErlNifResourceType* egl_context_resource_type = NULL;
static ErlNifResourceType* egl_client_buffer_resource_type = NULL;
static ErlNifResourceType* egl_sync_resource_type = NULL;
static ErlNifResourceType* egl_image_resource_type = NULL;

static void egl_display_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void egl_config_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void egl_surface_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void egl_context_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void egl_client_buffer_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void egl_sync_resource_dtor(ErlNifEnv* env, void* obj) {
}

static void egl_image_resource_dtor(ErlNifEnv* env, void* obj) {
}

static int nif_module_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM arg)
{
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

    return 0;
}

static int nif_module_unload(ErlNifEnv* caller_env, void** priv_data)
{
    return 0;
}

static ERL_NIF_TERM nif_foo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return enif_make_int(env, 42);
}

static ErlNifFunc nif_functions[] = {
    {"foo", 0, nif_foo}
};

ERL_NIF_INIT(
    egl,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
