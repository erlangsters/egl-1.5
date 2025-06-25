# OpenGL Command Executor

> This document is for developers who need to interpolate with the EGL binding
> to be able to execute OpenGL commands in the correct OS thread. For instance,
> the [OpenGL bindings](https://github.com/orgs/erlangsters/repositories?type=all&q=opengl-)
> make use of this technique.

As properly documented in the thread-safety [document](thread-safety.md), the
EGL binding spawns OS threads in a way that maps the BEAM processes, and
schedule execution of OpenGL commands on them.

This is what makes OpenGL on the BEAM possible.

This document explains how you could execute your own OpenGL commands using the
C API of the EGL binding.

## Recipe

To be written.

```c
this();
```

To be written.

```c
that();
```
