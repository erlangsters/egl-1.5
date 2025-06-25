# API Mapping

A low-level C API cannot translate the same in the higher-level Erlang and 
Elixir programming language. Therefore, adjustments have to be made.

This document explains the API mapping rules that were rigorously followed 
during the development of the binding.

## Idiomatic API

Perhaps the most obvious
This binding follows a very consistent set of mapping rules. All those rules 
are documented (unless trivial) and when exceptions are made, they're 
documented as well.

You do not need to really formally understand those rules as most of the times,
this is to make the API more idiomatic
Equipped with common sense and the API reference, you will not fight the 
binding.

## Mapping Rules

To be written.

- This
- That

- `eglGetProcAddress` not implemented

To be written.

## Functions Tables

Here is the list of all EGL functions, organized in alphabetical order, with 
their equivalent. If exceptions where to be made, it's in the notes.

**B**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglBindAPI` | `bind_api` | N/A |
| `eglBindTexImage` | `bind_tex_image` | XXX: To be implemented. |

**C**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglChooseConfig` | `choose_config` | XXX: To be implemented.|
| `eglClientWaitSync` | `client_wait_sync` | XXX: To be implemented. |
| `eglCopyBuffers` | `copy_buffers` | XXX: Not implemented yet. |
| `eglCreateContext` | `create_context` | N/A |
| `eglCreateImage` | `create_image` | XXX: Not implemented yet. |
| `eglCreatePbufferFromClientBuffer` | `create_pbuffer_from_client_buffer` | XXX: To be implemented. |
| `eglCreatePbufferSurface` | `create_pbuffer_surface` | N/A |
| `eglCreatePixmapSurface` | `create_pixmap_surface` | XXX: To be implemented. |
| `eglCreatePlatformPixmapSurface` | `create_platform_pixmap_surface` | XXX: To be implemented. |
| `eglCreatePlatformWindowSurface` | `create_platform_window_surface` | XXX: To be implemented. |
| `eglCreateSync` | `create_sync` | XXX: To be implemented. |
| `eglCreateWindowSurface` | `create_window_surface` | XXX: To be implemented. |

**D**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglDestroyContext` | `destroy_context` | N/A |
| `eglDestroyImage` | `destroy_image` | N/A |
| `eglDestroySurface` | `destroy_surface` | N/A |
| `eglDestroySync` | `destroy_sync` | XXX: To be implemented. |

**G**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglGetConfigAttrib` | `get_config_attrib` | N/A |
| `eglGetConfigs` | `get_configs` | N/A |
| `eglGetCurrentContext` | `get_current_context` | N/A |
| `eglGetCurrentDisplay` | `get_current_display` | N/A |
| `eglGetCurrentSurface` | `get_current_surface` | N/A |
| `eglGetDisplay` | `get_display` | N/A |
| `eglGetError` | `get_error` | N/A |
| `eglGetPlatformDisplay` | `get_platform_display` | XXX: Not implemented yet |
| `eglGetProcAddress` | N/A | N/A |
| `eglGetSyncAttrib` | `get_sync_attrib` | XXX: Not implemented yet |

**I**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglInitialize` | `initialize` | N/A |

**M**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglMakeCurrent` | `make_current` | N/A |

**Q**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglQueryAPI` | `query_api` | N/A |
| `eglQueryContext` | `query_context` | N/A |
| `eglQueryString` | `query_string` | N/A |
| `eglQuerySurface` | `query_surface` | N/A |

**R**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglReleaseTexImage` | `release_tex_image` | XXX: To be implemented. |
| `eglReleaseThread` | `release_thread` | N/A |

**S**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglSurfaceAttrib` | `surface_attrib` | N/A |
| `eglSwapBuffers` | `swap_buffers` | N/A |
| `eglSwapInterval` | `swap_interval` | N/A |

**T**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglTerminate` | `terminate` | N/A |

**W**

| EGL | Binding | Notes |
|---- | ------- | ----- |
| `eglWaitClient` | `wait_client` | N/A |
| `eglWaitGL` | `wait_gl` | N/A |
| `eglWaitNative` | `wait_native` | N/A |
| `eglWaitSync` | `wait_sync` | XXX: To be implemented. |
