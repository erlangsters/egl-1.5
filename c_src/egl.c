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

static ERL_NIF_TERM nif_choose_config(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglChooseConfig (EGLDisplay dpy, const EGLint *attrib_list, EGLConfig *configs, EGLint config_size, EGLint *num_config);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_copy_buffers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglCopyBuffers (EGLDisplay dpy, EGLSurface surface, EGLNativePixmapType target);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLContext EGLAPIENTRY eglCreateContext (EGLDisplay dpy, EGLConfig config, EGLContext share_context, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_pbuffer_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePbufferSurface (EGLDisplay dpy, EGLConfig config, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_pixmap_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePixmapSurface (EGLDisplay dpy, EGLConfig config, EGLNativePixmapType pixmap, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_window_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglCreateWindowSurface (EGLDisplay dpy, EGLConfig config, EGLNativeWindowType win, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglDestroyContext (EGLDisplay dpy, EGLContext ctx);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglDestroySurface (EGLDisplay dpy, EGLSurface surface);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_config_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglGetConfigAttrib (EGLDisplay dpy, EGLConfig config, EGLint attribute, EGLint *value);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_configs(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglGetConfigs (EGLDisplay dpy, EGLConfig *configs, EGLint config_size, EGLint *num_config);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_current_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLDisplay EGLAPIENTRY eglGetCurrentDisplay (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_current_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglGetCurrentSurface (EGLint readdraw);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLDisplay EGLAPIENTRY eglGetDisplay (EGLNativeDisplayType display_id);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLint EGLAPIENTRY eglGetError (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglInitialize (EGLDisplay dpy, EGLint *major, EGLint *minor);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_make_current(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglMakeCurrent (EGLDisplay dpy, EGLSurface draw, EGLSurface read, EGLContext ctx);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_query_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglQueryContext (EGLDisplay dpy, EGLContext ctx, EGLint attribute, EGLint *value);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_query_string(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI const char *EGLAPIENTRY eglQueryString (EGLDisplay dpy, EGLint name);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_query_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglQuerySurface (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint *value);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_swap_buffers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglSwapBuffers (EGLDisplay dpy, EGLSurface surface);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_terminate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglTerminate (EGLDisplay dpy);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_wait_gl(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglWaitGL (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_wait_native(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglWaitNative (EGLint engine);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_bind_tex_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglBindTexImage (EGLDisplay dpy, EGLSurface surface, EGLint buffer);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_release_tex_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglReleaseTexImage (EGLDisplay dpy, EGLSurface surface, EGLint buffer);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_surface_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglSurfaceAttrib (EGLDisplay dpy, EGLSurface surface, EGLint attribute, EGLint value);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_swap_interval(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglSwapInterval (EGLDisplay dpy, EGLint interval);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_bind_api(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglBindAPI (EGLenum api);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_query_api(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLenum EGLAPIENTRY eglQueryAPI (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_pbuffer_from_client_buffer(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePbufferFromClientBuffer (EGLDisplay dpy, EGLenum buftype, EGLClientBuffer buffer, EGLConfig config, const EGLint *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_release_thread(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglReleaseThread (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_wait_client(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglWaitClient (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_current_context(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLContext EGLAPIENTRY eglGetCurrentContext (void);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSync EGLAPIENTRY eglCreateSync (EGLDisplay dpy, EGLenum type, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglDestroySync (EGLDisplay dpy, EGLSync sync);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_client_wait_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLint EGLAPIENTRY eglClientWaitSync (EGLDisplay dpy, EGLSync sync, EGLint flags, EGLTime timeout);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_sync_attrib(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglGetSyncAttrib (EGLDisplay dpy, EGLSync sync, EGLint attribute, EGLAttrib *value);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLImage EGLAPIENTRY eglCreateImage (EGLDisplay dpy, EGLContext ctx, EGLenum target, EGLClientBuffer buffer, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_destroy_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglDestroyImage (EGLDisplay dpy, EGLImage image);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_get_platform_display(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLDisplay EGLAPIENTRY eglGetPlatformDisplay (EGLenum platform, void *native_display, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_platform_window_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePlatformWindowSurface (EGLDisplay dpy, EGLConfig config, void *native_window, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_create_platform_pixmap_surface(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLSurface EGLAPIENTRY eglCreatePlatformPixmapSurface (EGLDisplay dpy, EGLConfig config, void *native_pixmap, const EGLAttrib *attrib_list);

    return enif_make_atom(env, "ok");
}

static ERL_NIF_TERM nif_wait_sync(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // EGLAPI EGLBoolean EGLAPIENTRY eglWaitSync (EGLDisplay dpy, EGLSync sync, EGLint flags);

    return enif_make_atom(env, "ok");
}

static ErlNifFunc nif_functions[] = {
    // EGL 1.0
    {"choose_config", 5, nif_choose_config},
    {"copy_buffers", 3, nif_copy_buffers},
    {"create_context", 4, nif_create_context},
    {"create_pbuffer_surface", 3, nif_create_pbuffer_surface},
    {"create_pixmap_surface", 4, nif_create_pixmap_surface},
    {"create_window_surface", 4, nif_create_window_surface},
    {"destroy_context", 2, nif_destroy_context},
    {"destroy_surface", 2, nif_destroy_surface},
    {"get_config_attrib", 4, nif_get_config_attrib},
    {"get_configs", 4, nif_get_configs},
    {"get_current_display", 0, nif_get_current_display},
    {"get_current_surface", 1, nif_get_current_surface},
    {"get_display", 1, nif_get_display},
    {"get_error", 0, nif_get_error},
    {"initialize", 3, nif_initialize},
    {"make_current", 4, nif_make_current},
    {"query_context", 4, nif_query_context},
    {"query_string", 2, nif_query_string},
    {"query_surface", 4, nif_query_surface},
    {"swap_buffers", 2, nif_swap_buffers},
    {"terminate", 1, nif_terminate},
    {"wait_gl", 0, nif_wait_gl},
    {"wait_native", 1, nif_wait_native},
    // EGL 1.1
    {"bind_tex_image", 3, nif_bind_tex_image},
    {"release_tex_image", 3, nif_release_tex_image},
    {"surface_attrib", 4, nif_surface_attrib},
    {"swap_interval", 2, nif_swap_interval},
    // EGL 1.2
    {"bind_api", 1, nif_bind_api},
    {"query_api", 0, nif_query_api},
    {"create_pbuffer_from_client_buffer", 5, nif_create_pbuffer_from_client_buffer},
    {"release_thread", 0, nif_release_thread},
    {"wait_client", 0, nif_wait_client},
    // EGL 1.4
    {"get_current_context", 0, nif_get_current_context},
    // EGL 1.5
    {"create_sync", 3, nif_create_sync},
    {"destroy_sync", 2, nif_destroy_sync},
    {"client_wait_sync", 4, nif_client_wait_sync},
    {"get_sync_attrib", 4, nif_get_sync_attrib},
    {"create_image", 5, nif_create_image},
    {"destroy_image", 2, nif_destroy_image},
    {"get_platform_display", 3, nif_get_platform_display},
    {"create_platform_window_surface", 4, nif_create_platform_window_surface},
    {"create_platform_pixmap_surface", 4, nif_create_platform_pixmap_surface},
    {"wait_sync", 3, nif_wait_sync}
};

ERL_NIF_INIT(
    egl,
    nif_functions,
    nif_module_load,
    NULL,
    NULL,
    nif_module_unload
);
