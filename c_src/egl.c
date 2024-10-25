#include <string.h>
#include <erl_nif.h>
#include <EGL/egl.h>

ERL_NIF_TERM ok_atom;
ERL_NIF_TERM not_ok_atom;

static ErlNifResourceType* egl_display_resource_type = NULL;
static ErlNifResourceType* egl_config_resource_type = NULL;
static ErlNifResourceType* egl_surface_resource_type = NULL;
static ErlNifResourceType* egl_context_resource_type = NULL;
static ErlNifResourceType* egl_client_buffer_resource_type = NULL;
static ErlNifResourceType* egl_sync_resource_type = NULL;
static ErlNifResourceType* egl_image_resource_type = NULL;

ERL_NIF_TERM egl_no_display_atom;

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
    ok_atom = enif_make_atom(env, "ok");
    not_ok_atom = enif_make_atom(env, "not_ok");

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

    egl_no_display_atom = enif_make_atom(env, "no_display");

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

    return 0;
}

static int nif_module_unload(ErlNifEnv* caller_env, void** priv_data)
{
    return 0;
}

static ERL_NIF_TERM nif_choose_config(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // Second argument is a list of integers that was prepared on the Erlang
    // side so we just pass it as is to eglChooseConfig. We just have to
    // convert it into a C array and append EGL_NONE to it.

    void* display_resource;
    if (!enif_get_resource(env, argv[0], egl_display_resource_type, &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = *((EGLDisplay*)display_resource);

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
                void* config_resource = enif_alloc_resource(egl_config_resource_type, sizeof(EGLConfig));
                *((EGLConfig*)config_resource) = configs[i];
                ERL_NIF_TERM config_term = enif_make_resource(env, config_resource);
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
    void* display_resource;
    if (!enif_get_resource(env, argv[0], egl_display_resource_type, &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = *((EGLDisplay*)display_resource);

    void* config_resource;
    if (!enif_get_resource(env, argv[1], egl_config_resource_type, &config_resource)) {
        return enif_make_badarg(env);
    }
    EGLConfig config = *((EGLConfig*)config_resource);

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
    void* display_resource;
    if (!enif_get_resource(env, argv[0], egl_display_resource_type, &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = *((EGLDisplay*)display_resource);

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
                void* config_resource = enif_alloc_resource(egl_config_resource_type, sizeof(EGLConfig));
                *((EGLConfig*)config_resource) = configs[i];
                ERL_NIF_TERM config_term = enif_make_resource(env, config_resource);
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
    // XXX: First parameter must be read and used. Will be done in later
    //      revisions.

    EGLDisplay display = eglGetDisplay(EGL_DEFAULT_DISPLAY);

    if (display == EGL_NO_DISPLAY) {
        return enif_make_atom(env, "no_display");
    }
    else {
        void* display_resource = enif_alloc_resource(egl_display_resource_type, sizeof(EGLDisplay));
        *((EGLDisplay*)display_resource) = display;

        return enif_make_resource(env, display_resource);
    }
}

static ERL_NIF_TERM nif_get_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{

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
}

static ERL_NIF_TERM nif_initialize(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    void* display_resource;
    if (!enif_get_resource(env, argv[0], egl_display_resource_type, &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = *((EGLDisplay*)display_resource);

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
    EGLDisplay display;
    if (enif_is_identical(argv[0], egl_no_display_atom)) {
        display = EGL_NO_DISPLAY;
    }
    else {
        void* display_resource;
        if (!enif_get_resource(env, argv[0], egl_display_resource_type, &display_resource)) {
            return enif_make_badarg(env);
        }
        display = *((EGLDisplay*)display_resource);
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
    void* display_resource;
    if (!enif_get_resource(env, argv[0], egl_display_resource_type, &display_resource)) {
        return enif_make_badarg(env);
    }
    EGLDisplay display = *((EGLDisplay*)display_resource);

    EGLBoolean result = eglTerminate(display);
    if (result == EGL_TRUE) {
        return ok_atom;
    }
    else {
        return not_ok_atom;
    }
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
    {"choose_config_raw", 2, nif_choose_config},
    {"copy_buffers", 3, nif_copy_buffers},
    {"create_context", 4, nif_create_context},
    {"create_pbuffer_surface", 3, nif_create_pbuffer_surface},
    {"create_pixmap_surface", 4, nif_create_pixmap_surface},
    {"create_window_surface", 4, nif_create_window_surface},
    {"destroy_context", 2, nif_destroy_context},
    {"destroy_surface", 2, nif_destroy_surface},
    {"get_config_attrib_raw", 3, nif_get_config_attrib},
    {"get_configs", 1, nif_get_configs},
    {"get_current_display", 0, nif_get_current_display},
    {"get_current_surface", 1, nif_get_current_surface},
    {"get_display", 1, nif_get_display},
    {"get_error", 0, nif_get_error},
    {"initialize", 1, nif_initialize},
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
