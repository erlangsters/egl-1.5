# Thread-safety

The technique used to make OpenGL possible at the BEAM involves spawning OS 
threads to map the layout of the BEAM processes and execute the OpenGL commands 
on them.

This document explains the high-level concept and its internal implementation.

## High-level overview

The promise is that the Erlang and Elixir programmer should be able to think 
like if a BEAM process is equal to a OS thread, then all the thread-related 
specificities of OpenGL continues to apply.

It's advertised like that but it does not mean that each BEAM process must 
have a corresponding process. In fact, there must be as many spawned OS threads
as there are acitve OpenGL contexts.

Only the EGL binding is in position to do the job. 
Since all BEAM code relies on the EGL binding for OpenGL contexts, we distinguish

- create_context (or eglCreateContext)
- destroy_context (or eglDestroyContext)
- make_current (or eglMakeCurrent)

Those ultimately, the only functions that change the states. If the EGL
implementation and all BEAM code don't manually use EGL, the rest will remain
true.


## An EGL context equates an OS thread

For every created EGL context, a separate OS thread is started.

```
{ok, Context} = egl:create_context(Display, Config, no_context, Attribs).
```

After it's executed, internally, an OS thread that is ready to execute commands
has started.

When the EGL context is destroyed, the spawned OS thread is terminated.

```
egl:destroy_context(Context).
```

Internally, a table is maintained allowing translation in both direction.

For a given EGLContext*, we can retrieve the CommandExecutor

**Bar**

The only way to make a context current (or "bind to an OS thread") is to
call the.

```
egl:make_current()
```

Note that it can have  aside-effect, making in

When the EGL binding executes this function, it updates an internal table that
maps the set of active EGL context to the BEAM processes.

Later when a OpenGL command is to be executed, there is a table lookup to
know which thread ist should be executed.



In fact, to unbind, it's the same call

```
egl:make_current(no_context, blabla).
```

To be written.

**Solving dying processes**

To be written.