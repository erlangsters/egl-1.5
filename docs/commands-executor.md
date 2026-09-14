# OpenGL command executor

This extra is for sibling NIF authors. The generated OpenGL bindings use it so `gl*` calls run on the OS thread that holds the process's current EGL context. See [thread safety](thread-safety.md) for the model.

There is no Erlang API for the executor.

## Recipe

Compute the path to this binding's NIF library in Erlang and pass it into `erlang:load_nif/2`, the same way GLFW does for the window-handle symbol.

```erlang
EGLPrivDir = code:priv_dir(egl),
EGLNifLocation = filename:join(EGLPrivDir, "beam-egl").
```

In `nif_module_load`, open that library and resolve `egl_execute_command`:

```c
typedef ERL_NIF_TERM (*egl_execute_command_fn)(
    ERL_NIF_TERM (*function)(ErlNifEnv*, int, const ERL_NIF_TERM[]),
    ErlNifEnv* env,
    int argc,
    const ERL_NIF_TERM argv[]
);

void* handle = dlopen(beam_egl_so_path, RTLD_NOW);
egl_execute_command_fn egl_execute_command =
    dlsym(handle, "egl_execute_command");
```

Wrap each OpenGL NIF body:

```c
return egl_execute_command(nif_clear, env, argc, argv);
```

`egl_execute_command` looks up the calling process in the active-context map and runs `function` on that context's executor thread. If the process has no current context, it returns `badarg`.

On Windows, `LoadLibrary` / `GetProcAddress` replace `dlopen` / `dlsym`. The exported symbol is `egl_execute_command` in `priv/beam-egl`.
