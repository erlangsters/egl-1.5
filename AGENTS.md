# egl-1.5

- `egl-1.5` is a native binding in the `graphics-stack` family.
- Treat it as a near-release binding. Prefer focused edits that preserve API mapping, native resource safety, and cross-platform build behavior.
- Preserve the central positioning of the repository: hand-written EGL 1.5 binding for the BEAM that anchors the generated OpenGL bindings and composes with `glfw`.
- Keep Erlang-facing APIs close to EGL while mapping enums to atoms, bitfields to lists of atoms, and success or failure to `{ok, Result}` / `not_ok` / a specific error atom.
- Thread-safety, current-context semantics, and native resource lifetime are core concerns. Do not simplify those constraints away.
- Linux builds expect system packages such as `cmake` and `libegl-dev`. macOS and Windows use ANGLE via `ANGLE_INCLUDE_DIR` and `ANGLE_LIB_DIR`.
- Keep a single public module: `egl`.
- Public mapping lives in `docs/api-mapping.md`. Internal status and rationale live in the Binding Surface below. Do not dump the function table into these always-on rules.
- Keep `XXX` comments in source, tests, and public docs until the owning slice replaces them with a written decision here.
- Dummy NIF implementations that return `ok` without calling EGL are not part of the working surface.

## Binding Surface

This is the internal scope note for what the EGL binding exposes today, what
remains to land before first release, and what is intentionally outside the
public surface.

The public-facing mapping document lives in `docs/api-mapping.md`. This section
is the authoritative internal inventory. The completion sequence lives in the
family-workspace `EGL-PLAN.md`.

First release is the OpenGL (and ES) plus GLFW path. Full EGL 1.5 coverage is
not the goal.

## Status Vocabulary

- `implemented`: present in the repository, calls EGL, and is part of the
  current surface. Known defects do not change this word.
- `planned`: intended for the public surface but not landed yet, or present
  but unfinished in a way that first release still owes. Includes exports
  whose NIF returns `ok` without calling EGL.
- `deferred`: reasonable later, but not part of the first release target.
- `not planned`: intentionally out of scope unless a concrete use case changes
  the decision.

## Scope Rules

- Keep a single public module: `egl`.
- Context and surface creation belong here. GLFW creates contextless windows
  and passes an `egl_window` resource into `create_window_surface/4`.
- One OS command-executor thread per EGL context. `make_current` binds a
  BEAM process to that thread. OpenGL NIFs reach it through
  `egl_execute_command`.
- Native window handles enter through the `egl_window` resource type and
  `get_egl_window_resource_type()`. Do not publish raw native pointers as
  Erlang terms.
- `eglGetProcAddress` stays unimplemented. Extension loading is not this
  binding's job.
- When adding or changing NIF-backed functionality, update the Erlang API,
  the NIF, the relevant test, and the docs together.

## Family Inventory

| Family | Status | Notes |
| --- | --- | --- |
| Display | implemented | `get_display(default_display)` requires that atom and calls `eglGetDisplay(EGL_DEFAULT_DISPLAY)`. Native displays use `get_platform_display/3`. |
| Config | implemented | `choose_config`, `get_configs`, `get_config_attrib`. Tests exist. Mapping docs still say `choose_config` is unimplemented. |
| Context | implemented | `create_context`, `destroy_context`, `query_context`, `get_current_context`. Client API is `bind_api/1`. |
| Surface | implemented | Pbuffer and window surfaces work. Window attribs are packed. Pixmap and platform surfaces are unexported (deferred). |
| Current / thread | implemented | `make_current`, `bind_api`, `query_api`, `release_thread`, `wait_client`, `wait_gl`, `wait_native`. Unbind with no current context is `ok`. |
| Swap | implemented | `swap_buffers`, `swap_interval`. |
| Query | implemented | `query_string`, `query_surface`, `surface_attrib`, current display/surface. Current getters allocate a new resource each time. |
| Platform display | implemented | `wayland`, `x11`, `angle`. Attrib list is `[]` only. GLFW windows on Wayland need `glfw:display_egl_handle/0`, not `default_display`. |
| Image | deferred | Unexported. C stubs commented out. |
| Sync | deferred | Unexported. C stubs commented out. |
| Teximage / OpenVG buffer | deferred | Unexported. C stubs commented out. |
| Interpolation | implemented | C ABI: `get_egl_window_resource_type`, `get_egl_native_display_resource_type`, `egl_execute_command`. Used by `glfw` and the OpenGL NIFs. Docs are unfinished. |
| Documentation | planned | Mapping table exists and is stale. Thread-safety and command-executor extras are stubs. Many `-doc` blocks are `To be written`. |
| Tests | planned | eunit covers display, config, context, pbuffer, surface attribs, queries. CI runs only `egl_get_display_test`. No window-surface eunit (needs GLFW). |

## Planned For First Release

These are owed before calling the binding finished. They are not a single
patch.

| Item | Slice | Rationale |
| --- | --- | --- |
| Resource intern and poison | 4 | Destroy/`terminate` must invalidate terms. Current getters must return the interned handle. |
| CI pbuffer suite | 5 | Compile-only plus `egl_get_display_test` does not prove the core path. |
| Documentation | 6 | Mapping, extras, README, missing `-doc`. |

## Deferred

Reasonable later, not required to call the first release done.

| Item | Rationale |
| --- | --- |
| `EGLSync` | Fence/sync objects. No current GLFW or OpenGL-binding caller. |
| `EGLImage` | Image sharing. Needs an explicit public contract for client buffers. |
| Pixmap surfaces and `eglCopyBuffers` | Native pixmap handle recipe, same class as window handles, no caller. |
| `eglBindTexImage` / `eglReleaseTexImage` | Pbuffer-to-texture. Graphics-stack path uses OpenGL textures, not this. |
| `eglCreatePbufferFromClientBuffer` | OpenVG. Out of the OpenGL path. |
| OpenVG as a first-class API | `query_api` may still return `openvg_api`. Do not grow an OpenVG surface around that. |

## Not Planned

Intentionally outside the first public surface.

| Upstream | Rationale |
| --- | --- |
| `eglGetProcAddress` | Extension loading. The OpenGL bindings do not load extensions this way. |
| Raw native display, window, or pixmap pointers as Erlang terms | Resource types only. |
| Per-BEAM-process OS threads | The design is one executor thread per EGL context, not per process. |
| Publishing the command executor as an Erlang API | It is a C interpolation ABI for sibling NIFs. |

## Known Defects

Not design questions. Fix them in the slice that owns the family.

- Display, config, surface, and context resource dtors are no-ops.
  `get_current_context` and `get_current_surface` wrap the native pointer
  in a new resource. Slice 4.
- `docs/api-mapping.md` marks implemented functions (`choose_config`,
  `create_window_surface`, …) as unimplemented. Slice 6.

## Interpolation ABI

Sibling NIFs depend on two exported C symbols:

- `get_egl_window_resource_type` — GLFW wraps a native window in an
  `egl_window` resource that `create_window_surface/4` accepts.
- `get_egl_native_display_resource_type` — GLFW wraps the window-system
  display in an `egl_native_display` resource that
  `get_platform_display/3` accepts.
- `egl_execute_command` — OpenGL NIFs run on the executor thread of the
  calling process's current context. No current context is `badarg` today.

Do not break those symbols without updating `glfw` and the OpenGL generator
in the same piece of work. The Erlang module does not export them.

## Open Questions Owned By Later Slices

These are the surviving questions, recorded so they are not lost.
Source and public-doc `XXX` comments remain until the owning slice lands.

- How `egl_execute_command` should report “no current context” (`badarg`
  vs a dedicated atom).
- Completeness and consistency of already-written `-doc` bodies.
- ANGLE display attribs (`EGL_PLATFORM_ANGLE_TYPE_*`). Attrib lists stay
  `[]` for now.
