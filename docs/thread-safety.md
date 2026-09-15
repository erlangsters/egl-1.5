# Thread safety

OpenGL commands need a current context on the calling OS thread. The BEAM runs Erlang code on arbitrary scheduler threads. This binding makes that combination usable: you can treat a BEAM process as if it were an OS thread for current-context rules.

It is not one OS thread per BEAM process. It is one command-executor OS thread per EGL context.

## One executor per context

`create_context/4` starts an OS thread for that context. `destroy_context/2` joins it. `terminate/1` joins every executor that belongs to the display.

```erlang
{ok, Context} = egl:create_context(Display, Config, no_context, Attribs).
ok = egl:destroy_context(Display, Context).
```

## Binding a process to a context

`make_current/4` records that the calling process owns that context and runs `eglMakeCurrent` on the context's executor.

`eglBindAPI` is per-OS-thread. `bind_api/1` before `make_current/4` runs on the calling scheduler thread, which is what `create_context/4` needs. `make_current/4` also binds the context's client API on the executor. After that, `bind_api/1` follows the executor so later API switches land on the same OS thread as OpenGL commands.

```erlang
ok = egl:make_current(Display, Surface, Surface, Context).
```

Unbind with `no_context`. It is `ok` if this process had no current context.

```erlang
ok = egl:make_current(Display, no_surface, no_surface, no_context).
```

`get_current_context/0`, `get_current_display/0`, and `get_current_surface/1` follow that pid map. They do not call `eglGetCurrent*` on a scheduler thread.

## OpenGL commands

Generated OpenGL NIFs do not call `gl*` on the scheduler thread. They call `egl_execute_command`, which looks up the calling process's current context and runs on that executor. If the process has no current context, the call is `badarg`.

See [command executor](commands-executor.md) if you are writing a sibling NIF.

## What still applies

There is still one current context per process, and making a context current here can steal it from another process the same way OpenGL steals it from another OS thread. Destroy the context (or terminate the display) explicitly. Dropping the last Erlang reference does not join the executor: resource destructors do not call EGL.
