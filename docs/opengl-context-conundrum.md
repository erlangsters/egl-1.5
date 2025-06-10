# OpenGL context Conundrum

Given OpenGL is thread "sensitive" and the BEAM implements its own "threading
model" (called Erlang processes), you will inevitably have to make sense of
this mess first.

Worry not. It's simple.

The EGL 1.5 and OpenGL bindings implement a layer to map Erlang processes to
OS threads. This means that you can simply think just like you would do C,
except that C threads become Erlang processes.

Consider the following example.

```erlang
Display = egl:get_display(default_display),
{ok, _} = egl:initialize(Display),

{ok, Context} = egl:create_context(Display, Config, no_context, [{context_major_version, 3}]),

Surface = create_surface(Display, Context),

egl:make_current(Display, Surface, Surface, Context),

% OpenGL calls here
```

This code is running in a given Erlang process and as soon as `egl:make_current/4`
is called, the OpenGL context is bound to this Erlang process. Therefore, all
OpenGL calls operating on this context must be made in this Erlang process.

Just like in C.

If you want to carry the rendering in another Erlang process, pass the context
and make it current in this new process. It will of course invalidate all the
OpenGL calls made in the previous process.

```
spawn(fun() ->
    egl:make_current(Display, Surface, Surface, Context),
    % OpenGL calls here
end).
```

Just like in C.

**Conclusion:** All the OpenGL context rules with respect to threads apply in
Erlang in the exact same way.

In some way, it'a bad news, because you still have to pay attention to
"thread-safety".

In another way, it's good news as you don't have to learn anything new, all C
code can be ported to Erlang with minimal changes, and most importantly, it's
a lot easier to write "thread-safe" code in Erlang than in C.

## Writing native code

The magic that allows writing code just like if an Erlang process was an OS 
thread is the public `egl_execute_command` C function provided by the EGL 
binding.

The OpenGL bindings have no knowledge of OpenGL contexts and what Erlang 
process or OS thread they may be bound to. They do not care. Instead, they 
provide a NIF function that encapsulates the OpenGL call and let EGL execute 
in the right OS thread.

```

static ERL_NIF_TERM nif_opengl_stuff_inner(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // This C function is executed in the right OS thread. So if the Erlang 
    // process has an OpenGL context virtually bound to it, this OS thread has 
    // the OpenGL context bound (actually).
    const char* gl_version = (const char*)glGetString(GL_VERSION);
    printf("OpenGL Version: %s\n", gl_version);

    return enif_make_atom(env, "doing_opengl_stuff");
}

static ERL_NIF_TERM nif_opengl_stuff(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    egl_execute_command(nif_opengl_stuff_inner, env, argc, argv);
}
```

Why are we talking implementation?

Because if you want to write a NIF function that performs OpenGL calls, you 
will want to do the same. For instance, if you write my_gl_stuff:heavy_work/0 
and call if from an Erlang process that has an OpenGL context, .





## Internal solutions

OpenGL in Erlang is cool but... achieving thread-safety in the BEAM emulator is 
not straightforward.

This document explains the problems and the possible solutions.

## The problem

Any graphics developer know that OpenGL operations are contextual. For 
instance, calling `glClear(...)` will affect the framebuffer associated with 
the current context, be it a window surface or something else. OpenGL contexts 
are created with libraries like EGL where each context can be only active on 
one CPU thread at a time.

Being a binding for the Erlang programming language, all OpenGL calls will then 
start from Erlang.

```
gl:do_this().
gl:do_that()
```

And end up in C inside a NIF functions.

```
static ERL_NIF_TERM gl_do_this(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glDoThis();
}
static ERL_NIF_TERM gl_do_that(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    glDoThat();
}
```

The BEAM emulator runs several emulator threads and it dispatches the calls 
arbitrarily. As OpenGL contexts are tied to CPU threads, simply executing the 
corresponding OpenGL function is not enough. The context must be adjusted 
first.

# Solution #1

Restricting BEAM emulator to one emulator thread. It can be achieved by passing 
the `+S` flag.

```erlang
erl +S 1
```

It may be silly, but it's already guaranteeing that the NIF functions will 
always run from the same CPU thread.

To be written.

# Solution #2

Each Erlang process which 

Re-establishing the OpenGL context before executing the corresponding OpenGL
function.

It's unsatisfying situation because OpenGL application are potentially made of
several thousands of OpenGL calls that will likely all run from different 
emulator threads. Checking for context and calling eglMakeCurrent if need to be
before each call is way too expensive.



# Solution #3

Spawning one CPU thread per OpenGL context and forwarding the execution to 
that thread.
