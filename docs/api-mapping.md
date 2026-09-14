# API mapping

This is a binding of EGL 1.5. Existing EGL knowledge still applies. The mapping is small, consistent, and documented here when it is not obvious.

The public module is `egl`. First release is the OpenGL (and ES) plus GLFW path. Pixmap, image, sync, teximage, and OpenVG client buffers are not exported.

## Mapping rules

- EGL enums become atoms whenever a closed set exists. For instance, `bind_api/1` takes `opengl_api | opengl_es_api | openvg_api`.
- EGL bitfields become lists of atoms. For instance, `{surface_type, [window_bit, pbuffer_bit]}`.
- An integer that EGL documents as only `EGL_TRUE` or `EGL_FALSE` becomes `boolean()`, or `ok` / `not_ok` when that is the C success/failure shape.
- `EGL_NO_DISPLAY`, `EGL_NO_SURFACE`, and `EGL_NO_CONTEXT` become `no_display`, `no_surface`, and `no_context`.
- `eglGetError` stays `get_error/0`. It returns atoms such as `success`, `bad_alloc`, and `bad_match`.
- `eglGetProcAddress` is not implemented. The OpenGL bindings do not load extensions this way.
- Display, config, surface, and context resources intern by native pointer. Destroy and `terminate/1` poison them; later calls raise `badarg`.
- `get_current_context/0`, `get_current_display/0`, and `get_current_surface/1` follow the process's `make_current/4` bind, not `eglGetCurrent*` on a scheduler thread.

## Display

| EGL | Binding | Notes |
| --- | --- | --- |
| `eglGetDisplay` | `get_display/1` | Argument must be `default_display`. Native displays use `get_platform_display/3`. |
| `eglGetPlatformDisplay` | `get_platform_display/3` | Platforms: `wayland`, `x11`, `angle`. Native display is `default_display` or an `egl_native_display` resource. Attrib list is `[]`. |
| `eglInitialize` | `initialize/1` | Returns `{ok, {Major, Minor}}`. |
| `eglTerminate` | `terminate/1` | Poisons the display and its interned configs, surfaces, and contexts. |
| `eglGetError` | `get_error/0` | N/A |
| `eglQueryString` | `query_string/2` | Display may be `no_display` for `version` and `extensions`. |

## Config

| EGL | Binding | Notes |
| --- | --- | --- |
| `eglGetConfigs` | `get_configs/1` | Interned configs. |
| `eglChooseConfig` | `choose_config/2` | Attribute list of `{Name, Value}` tuples. Bitfields are lists of atoms. |
| `eglGetConfigAttrib` | `get_config_attrib/3` | N/A |

## Surface

| EGL | Binding | Notes |
| --- | --- | --- |
| `eglCreateWindowSurface` | `create_window_surface/4` | Native window is an `egl_window` resource. Attribs are packed; `[]` is NULL. |
| `eglCreatePbufferSurface` | `create_pbuffer_surface/3` | N/A |
| `eglCreatePixmapSurface` | N/A | Deferred. |
| `eglCreatePlatformWindowSurface` | N/A | Window surfaces stay `create_window_surface/4`. |
| `eglCreatePlatformPixmapSurface` | N/A | Deferred. |
| `eglDestroySurface` | `destroy_surface/2` | Poisons the surface. |
| `eglQuerySurface` | `query_surface/3` | N/A |
| `eglSurfaceAttrib` | `surface_attrib/4` | N/A |
| `eglBindTexImage` | N/A | Deferred. |
| `eglReleaseTexImage` | N/A | Deferred. |
| `eglCreatePbufferFromClientBuffer` | N/A | Deferred. |
| `eglCopyBuffers` | N/A | Deferred. |

## Context

| EGL | Binding | Notes |
| --- | --- | --- |
| `eglBindAPI` | `bind_api/1` | Client API is not forced inside `create_context/4`. |
| `eglQueryAPI` | `query_api/0` | N/A |
| `eglCreateContext` | `create_context/4` | Starts the command-executor thread. Share may be `no_context`. |
| `eglDestroyContext` | `destroy_context/2` | Joins the executor and poisons the context. |
| `eglMakeCurrent` | `make_current/4` | Draw/read may be `no_surface`. Context may be `no_context` to unbind. |
| `eglGetCurrentContext` | `get_current_context/0` | From the pid map. |
| `eglGetCurrentDisplay` | `get_current_display/0` | From the pid map. |
| `eglGetCurrentSurface` | `get_current_surface/1` | `draw` or `read`. From the pid map. |
| `eglQueryContext` | `query_context/3` | N/A |
| `eglWaitClient` | `wait_client/0` | N/A |
| `eglWaitGL` | `wait_gl/0` | N/A |
| `eglWaitNative` | `wait_native/1` | Argument is `core_native_engine`. |
| `eglReleaseThread` | `release_thread/0` | N/A |

## Swap

| EGL | Binding | Notes |
| --- | --- | --- |
| `eglSwapBuffers` | `swap_buffers/2` | Runs on the current context's executor. |
| `eglSwapInterval` | `swap_interval/2` | N/A |

## Not implemented

| EGL | Binding | Notes |
| --- | --- | --- |
| `eglGetProcAddress` | N/A | Extension loading is out of scope. |
| `eglCreateSync` / `eglDestroySync` / `eglClientWaitSync` / `eglGetSyncAttrib` / `eglWaitSync` | N/A | Deferred. |
| `eglCreateImage` / `eglDestroyImage` | N/A | Deferred. |
