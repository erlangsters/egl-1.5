# Native Window Handle

> This document is for developers who need to interpolate with the EGL binding
> to be able to provide a native window handle. For instance, the
> [GLFW binding](https://github.com/erlangsters/glfw) makes use of this
technique.

Besides providing cross-platform OpenGL contexts, EGL also abstracts away the
platform-specific window systems with its `EGLNativeWindowType` type. How would
you give the EGL binding a valid native window handle ? This document explains the recipe.

## Recipe

The EGL binding wraps a native window handle using the `ErlNifResourceType`
internally named `egl_window`.

```
ErlNifResourceType* egl_window_resource_type = enif_open_resource_type(env, NULL, "egl_window", NULL, ERL_NIF_RT_CREATE, NULL);
```

You need access to this internal `ErlNifResourceType` for your binding to be
able to provide the EGL binding a native window handle it can work with. To
make it possible, it exposes the `get_egl_window_resource_type()` function
which allows you to retrieve it.

Once the `ErlNifResourceType` retrieved, you become in position to create a EGL
window term.

```c
// This is your function.
EGLNativeWindowType window_handle = get_native_window_handle();

void* egl_window_resource = enif_alloc_resource(egl_window_resource_type, sizeof(EGLNativeWindowType));
*((EGLNativeWindowType*)egl_window_resource) = window_handle;

ERL_NIF_TERM resource_term = enif_make_resource(env, egl_window_resource);
```

This returned term will happily be accepted by the `create_window_surface/4`
function.

Now, to access the `get_egl_window_resource_type()` function from your C code,
you will need to manually open the binding NIF library (named `beam-egl.so` on
Linux, for instance) and load the the symbol.

Let's see how we can do this.

**Loading the symbol**

First off, you gotta compute somehow the location of the EGL binding library.
It can be done in Erlang before the NIF module loads the NIF library.

```
EGLPrivDir = code:priv_dir(egl),
EGLNifLocation = filename:join(EGLPrivDir, "beam-egl") ++ ".so",
```

Then later pass as an argument.

```
erlang:load_nif(SoName, EGLNifLocation).
```

In the `nif_module_load` function of your NIF library, you can retrieve a
pointer to the `get_egl_window_resource_type()` function which allows to
retrieve the ``ErlNifResourceType`.

```c
typedef ErlNifResourceType* (*get_egl_window_resource_type_fn)(ErlNifEnv*);
```
```c
char beam_egl_so_path[1024];
if (!enif_get_string(env, arg, beam_egl_so_path, sizeof(beam_egl_so_path), ERL_NIF_LATIN1)) {
    fprintf(stderr, "failed to read EGL binding library path from argument\n");
    return -1;
}

void* egl_nif_lib_handle = dlopen(beam_egl_so_path, RTLD_NOW | RTLD_GLOBAL);
if (!egl_nif_lib_handle) {
    fprintf(stderr, "failed to load beam-egl.so: %s\n", dlerror());
    return -1;
}

get_egl_window_resource_type_fn get_egl_window_resource_type = dlsym(egl_nif_lib_handle, "get_egl_window_resource_type");
if (!get_egl_window_resource_type) {
    fprintf(stderr, "failed to load symbol get_egl_window_resource_type: %s\n", dlerror());
    dlclose(egl_nif_lib_handle);
    return -1;
}

// Note that the argument parameter is unused (it's only used during the first
// call which is already made by the EGL binding).
ErlNifResourceType* egl_window_resource_type = get_egl_window_resource_type(env);
```

And voila.
