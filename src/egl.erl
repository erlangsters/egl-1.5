%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(egl).

-export_type([
    display/0,
    config/0,
    surface/0,
    context/0,
    client_buffer/0,
    sync/0,
    image/0,
    config_attrib/0,
    config_attribs_list/0
]).
-export([
    choose_config/2,
    copy_buffers/3,
    create_context/4,
    create_pbuffer_surface/3,
    create_pixmap_surface/4,
    create_window_surface/4,
    destroy_context/2,
    destroy_surface/2,
    get_config_attrib/3,
    get_configs/1,
    get_current_display/0,
    get_current_surface/1,
    get_display/1,
    get_error/0,
    initialize/1,
    make_current/4,
    query_context/3,
    query_string/2,
    query_surface/3,
    swap_buffers/2,
    terminate/1,
    wait_gl/0,
    wait_native/1,
    bind_tex_image/3,
    release_tex_image/3,
    surface_attrib/4,
    swap_interval/2,
    bind_api/1,
    query_api/0,
    create_pbuffer_from_client_buffer/5,
    release_thread/0,
    wait_client/0,
    get_current_context/0,
    create_sync/3,
    destroy_sync/2,
    client_wait_sync/4,
    get_sync_attrib/4,
    create_image/5,
    destroy_image/2,
    get_platform_display/3,
    create_platform_window_surface/4,
    create_platform_pixmap_surface/4,
    wait_sync/3
]).

-nifs([
    choose_config_raw/2,
    copy_buffers/3,
    create_context_raw/4,
    create_pbuffer_surface_raw/3,
    create_pixmap_surface/4,
    create_window_surface/4,
    destroy_context/2,
    destroy_surface/2,
    get_config_attrib_raw/3,
    get_configs/1,
    get_current_display/0,
    get_current_surface_raw/1,
    get_display/1,
    get_error/0,
    initialize/1,
    make_current/4,
    query_context_raw/3,
    query_string/2,
    query_surface_raw/3,
    swap_buffers/2,
    terminate/1,
    wait_gl/0,
    wait_native/1,
    bind_tex_image/3,
    release_tex_image/3,
    surface_attrib_raw/4,
    swap_interval/2,
    bind_api_raw/1,
    query_api_raw/0,
    create_pbuffer_from_client_buffer/5,
    release_thread/0,
    wait_client/0,
    get_current_context/0,
    create_sync/3,
    destroy_sync/2,
    client_wait_sync/4,
    get_sync_attrib/4,
    create_image/5,
    destroy_image/2,
    get_platform_display/3,
    create_platform_window_surface/4,
    create_platform_pixmap_surface/4,
    wait_sync/3
]).

-on_load(init/0).

% EGL 1.0.
-define(EGL_ALPHA_SIZE, 16#3021).
-define(EGL_BAD_ACCESS, 16#3002).
-define(EGL_BAD_ALLOC, 16#3003).
-define(EGL_BAD_ATTRIBUTE, 16#3004).
-define(EGL_BAD_CONFIG, 16#3005).
-define(EGL_BAD_CONTEXT, 16#3006).
-define(EGL_BAD_CURRENT_SURFACE, 16#3007).
-define(EGL_BAD_DISPLAY, 16#3008).
-define(EGL_BAD_MATCH, 16#3009).
-define(EGL_BAD_NATIVE_PIXMAP, 16#300A).
-define(EGL_BAD_NATIVE_WINDOW, 16#300B).
-define(EGL_BAD_PARAMETER, 16#300C).
-define(EGL_BAD_SURFACE, 16#300D).
-define(EGL_BLUE_SIZE, 16#3022).
-define(EGL_BUFFER_SIZE, 16#3020).
-define(EGL_CONFIG_CAVEAT, 16#3027).
-define(EGL_CONFIG_ID, 16#3028).
-define(EGL_CORE_NATIVE_ENGINE, 16#305B).
-define(EGL_DONT_CARE, -1).
-define(EGL_DEPTH_SIZE, 16#3025).
-define(EGL_DRAW, 16#3059).
-define(EGL_EXTENSIONS, 16#3055).
-define(EGL_FALSE, 0).
-define(EGL_GREEN_SIZE, 16#3023).
-define(EGL_HEIGHT, 16#3056).
-define(EGL_LARGEST_PBUFFER, 16#3058).
-define(EGL_LEVEL, 16#3029).
-define(EGL_MAX_PBUFFER_HEIGHT, 16#302A).
-define(EGL_MAX_PBUFFER_PIXELS, 16#302B).
-define(EGL_MAX_PBUFFER_WIDTH, 16#302C).
-define(EGL_NATIVE_RENDERABLE, 16#302D).
-define(EGL_NATIVE_VISUAL_ID, 16#302E).
-define(EGL_NATIVE_VISUAL_TYPE, 16#302F).
-define(EGL_NONE, 16#3038).
-define(EGL_NON_CONFORMANT_CONFIG, 16#3051).
-define(EGL_NOT_INITIALIZED, 16#3001).
-define(EGL_PBUFFER_BIT, 16#0001).
-define(EGL_PIXMAP_BIT, 16#0002).
-define(EGL_READ, 16#305A).
-define(EGL_RED_SIZE, 16#3024).
-define(EGL_SAMPLES, 16#3031).
-define(EGL_SAMPLE_BUFFERS, 16#3032).
-define(EGL_SLOW_CONFIG, 16#3050).
-define(EGL_STENCIL_SIZE, 16#3026).
-define(EGL_SUCCESS, 16#3000).
-define(EGL_SURFACE_TYPE, 16#3033).
-define(EGL_TRANSPARENT_BLUE_VALUE, 16#3035).
-define(EGL_TRANSPARENT_GREEN_VALUE, 16#3036).
-define(EGL_TRANSPARENT_RED_VALUE, 16#3037).
-define(EGL_TRANSPARENT_RGB, 16#3052).
-define(EGL_TRANSPARENT_TYPE, 16#3034).
-define(EGL_TRUE, 1).
-define(EGL_VENDOR, 16#3053).
-define(EGL_VERSION, 16#3054).
-define(EGL_WIDTH, 16#3057).
-define(EGL_WINDOW_BIT, 16#0004).

% EGL 1.1.
-define(EGL_BACK_BUFFER, 16#3084).
-define(EGL_BIND_TO_TEXTURE_RGB, 16#3039).
-define(EGL_BIND_TO_TEXTURE_RGBA, 16#303A).
-define(EGL_CONTEXT_LOST, 16#300E).
-define(EGL_MIN_SWAP_INTERVAL, 16#303B).
-define(EGL_MAX_SWAP_INTERVAL, 16#303C).
-define(EGL_MIPMAP_TEXTURE, 16#3082).
-define(EGL_MIPMAP_LEVEL, 16#3083).
-define(EGL_NO_TEXTURE, 16#305C).
-define(EGL_TEXTURE_2D, 16#305F).
-define(EGL_TEXTURE_FORMAT, 16#3080).
-define(EGL_TEXTURE_RGB, 16#305D).
-define(EGL_TEXTURE_RGBA, 16#305E).
-define(EGL_TEXTURE_TARGET, 16#3081).

% EGL 1.2.
-define(EGL_ALPHA_FORMAT, 16#3088).
-define(EGL_ALPHA_FORMAT_NONPRE, 16#308B).
-define(EGL_ALPHA_FORMAT_PRE, 16#308C).
-define(EGL_ALPHA_MASK_SIZE, 16#303E).
-define(EGL_BUFFER_PRESERVED, 16#3094).
-define(EGL_BUFFER_DESTROYED, 16#3095).
-define(EGL_CLIENT_APIS, 16#308D).
-define(EGL_COLORSPACE, 16#3087).
-define(EGL_COLORSPACE_sRGB, 16#3089).
-define(EGL_COLORSPACE_LINEAR, 16#308A).
-define(EGL_COLOR_BUFFER_TYPE, 16#303F).
-define(EGL_CONTEXT_CLIENT_TYPE, 16#3097).
-define(EGL_DISPLAY_SCALING, 10000).
-define(EGL_HORIZONTAL_RESOLUTION, 16#3090).
-define(EGL_LUMINANCE_BUFFER, 16#308F).
-define(EGL_LUMINANCE_SIZE, 16#303D).
-define(EGL_OPENGL_ES_BIT, 16#0001).
-define(EGL_OPENVG_BIT, 16#0002).
-define(EGL_OPENGL_ES_API, 16#30A0).
-define(EGL_OPENVG_API, 16#30A1).
-define(EGL_OPENVG_IMAGE, 16#3096).
-define(EGL_PIXEL_ASPECT_RATIO, 16#3092).
-define(EGL_RENDERABLE_TYPE, 16#3040).
-define(EGL_RENDER_BUFFER, 16#3086).
-define(EGL_RGB_BUFFER, 16#308E).
-define(EGL_SINGLE_BUFFER, 16#3085).
-define(EGL_SWAP_BEHAVIOR, 16#3093).
-define(EGL_VERTICAL_RESOLUTION, 16#3091).

% EGL 1.3.
-define(EGL_CONFORMANT, 16#3042).
-define(EGL_CONTEXT_CLIENT_VERSION, 16#3098).
-define(EGL_MATCH_NATIVE_PIXMAP, 16#3041).
-define(EGL_OPENGL_ES2_BIT, 16#0004).
-define(EGL_VG_ALPHA_FORMAT, 16#3088).
-define(EGL_VG_ALPHA_FORMAT_NONPRE, 16#308B).
-define(EGL_VG_ALPHA_FORMAT_PRE, 16#308C).
-define(EGL_VG_ALPHA_FORMAT_PRE_BIT, 16#0040).
-define(EGL_VG_COLORSPACE, 16#3087).
-define(EGL_VG_COLORSPACE_sRGB, 16#3089).
-define(EGL_VG_COLORSPACE_LINEAR, 16#308A).
-define(EGL_VG_COLORSPACE_LINEAR_BIT, 16#0020).

% EGL 1.4.
-define(EGL_MULTISAMPLE_RESOLVE_BOX_BIT, 16#0200).
-define(EGL_MULTISAMPLE_RESOLVE, 16#3099).
-define(EGL_MULTISAMPLE_RESOLVE_DEFAULT, 16#309A).
-define(EGL_MULTISAMPLE_RESOLVE_BOX, 16#309B).
-define(EGL_OPENGL_API, 16#30A2).
-define(EGL_OPENGL_BIT, 16#0008).
-define(EGL_SWAP_BEHAVIOR_PRESERVED_BIT, 16#0400).

% EGL 1.5.
-define(EGL_CONTEXT_MAJOR_VERSION, 16#3098).
-define(EGL_CONTEXT_MINOR_VERSION, 16#30FB).
-define(EGL_CONTEXT_OPENGL_PROFILE_MASK, 16#30FD).
-define(EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY, 16#31BD).
-define(EGL_NO_RESET_NOTIFICATION, 16#31BE).
-define(EGL_LOSE_CONTEXT_ON_RESET, 16#31BF).
-define(EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT, 16#00000001).
-define(EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT, 16#00000002).
-define(EGL_CONTEXT_OPENGL_DEBUG, 16#31B0).
-define(EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE, 16#31B1).
-define(EGL_CONTEXT_OPENGL_ROBUST_ACCESS, 16#31B2).
-define(EGL_OPENGL_ES3_BIT, 16#00000040).
-define(EGL_CL_EVENT_HANDLE, 16#309C).
-define(EGL_SYNC_CL_EVENT, 16#30FE).
-define(EGL_SYNC_CL_EVENT_COMPLETE, 16#30FF).
-define(EGL_SYNC_PRIOR_COMMANDS_COMPLETE, 16#30F0).
-define(EGL_SYNC_TYPE, 16#30F7).
-define(EGL_SYNC_STATUS, 16#30F1).
-define(EGL_SYNC_CONDITION, 16#30F8).
-define(EGL_SIGNALED, 16#30F2).
-define(EGL_UNSIGNALED, 16#30F3).
-define(EGL_SYNC_FLUSH_COMMANDS_BIT, 16#0001).
-define(EGL_FOREVER, 16#FFFFFFFFFFFFFFFF).
-define(EGL_TIMEOUT_EXPIRED, 16#30F5).
-define(EGL_CONDITION_SATISFIED, 16#30F6).
-define(EGL_SYNC_FENCE, 16#30F9).
-define(EGL_GL_COLORSPACE, 16#309D).
-define(EGL_GL_COLORSPACE_SRGB, 16#3089).
-define(EGL_GL_COLORSPACE_LINEAR, 16#308A).
-define(EGL_GL_RENDERBUFFER, 16#30B9).
-define(EGL_GL_TEXTURE_2D, 16#30B1).
-define(EGL_GL_TEXTURE_LEVEL, 16#30BC).
-define(EGL_GL_TEXTURE_3D, 16#30B2).
-define(EGL_GL_TEXTURE_ZOFFSET, 16#30BD).
-define(EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_X, 16#30B3).
-define(EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_X, 16#30B4).
-define(EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Y, 16#30B5).
-define(EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Y, 16#30B6).
-define(EGL_GL_TEXTURE_CUBE_MAP_POSITIVE_Z, 16#30B7).
-define(EGL_GL_TEXTURE_CUBE_MAP_NEGATIVE_Z, 16#30B8).
-define(EGL_IMAGE_PRESERVED, 16#30D2).

-type display() :: reference().
-type config() :: reference().
-type surface() :: reference().
-type context() :: reference().
-type client_buffer() :: reference().
-type sync() :: reference().
-type image() :: reference().

-type config_attrib() ::
    alpha_size |
    alpha_mask_size |
    bind_to_texture_rgb |
    bind_to_texture_rgba |
    blue_size |
    buffer_size |
    color_buffer_type |
    config_caveat |
    config_id |
    conformant |
    depth_size |
    green_size |
    level |
    luminance_size |
    max_pbuffer_width |
    max_pbuffer_height |
    max_pbuffer_pixels |
    max_swap_interval |
    min_swap_interval |
    native_renderable |
    native_visual_id |
    native_visual_type |
    red_size |
    renderable_type |
    sample_buffers |
    samples |
    stencil_size |
    surface_type |
    transparent_type |
    transparent_red_value |
    transparent_green_value |
    transparent_blue_value
.

-type config_attribs_list() :: [
    {alpha_mask_size, pos_integer()} |
    {alpha_size, pos_integer()} |
    {bind_to_texture_rgb, dont_care | boolean()} |
    {bind_to_texture_rgba, dont_care | boolean()} |
    {blue_size, pos_integer()} |
    {buffer_size, pos_integer()} |
    {color_buffer_type, rgb_buffer | luminance_buffer} |
    {config_caveat, dont_care | none | slow_config} |
    {config_id, integer()} |
    {conformant, [opengl_bit | opengl_es_bit | opengl_es2_bit | openvg_bit]} |
    {depth_size, pos_integer()} |
    {green_size, pos_integer()} |
    {level, integer()} |
    {luminance_size, pos_integer()} |
    {native_renderable, dont_care | boolean()} |
    {max_swap_interval, dont_care | integer()} |
    {min_swap_interval, dont_care | integer()} |
    {red_size, pos_integer()} |
    {sample_buffers, pos_integer()} |
    {samples, pos_integer()} |
    {stencil_size, pos_integer()} |
    {renderable_type, [opengl_bit | opengl_es_bit | opengl_es2_bit | openvg_bit]} |
    {surface_type, [
        multisample_resolve_box_bit | pbuffer_bit | pixmat_bit |
        swap_behavior_preserved_bit | vg_alpha_format_pre_bit |
        vg_colorspace_linear_bit | window_bit
    ]} |
    {transparent_type, none | transparent_rgb} |
    {transparent_red_value, dont_care | pos_integer()} |
    {transparent_green_value, dont_care | pos_integer()} |
    {transparent_blue_value, dont_care | pos_integer()}
].

-type surface_attribs_list() :: [
    {gl_colorspace, gl_colorspace_srgb | gl_colorspace_linear} |
    {height, pos_integer()} |
    {largest_pbuffer, boolean()} |
    {mipmap_texture, boolean()} |
    {texture_format, no_texture | texture_rgb | texture_rgba} |
    {texture_target, no_texture | texture_2d} |
    {vg_alpha_format, vg_alpha_format_nonpre | vg_alpha_format_pre} |
    {vg_colorspace, vg_colorspace_srgb | vg_colorspace_linear} |
    {width, pos_integer()}
].
-type surface_attrib_get() ::
    config_id |
    gl_colorspace |
    height |
    horizontal_resolution |
    largest_pbuffer |
    mipmap_level |
    mipmap_texture |
    multisample_resolve |
    pixel_aspect_ratio |
    render_buffer |
    swap_behavior |
    texture_format |
    texture_target |
    vertical_resolution |
    vg_alpha_format |
    vg_colorspace |
    width
.
-type surface_attrib_set() ::
    mipmap_level |
    multisample_resolve |
    swap_behavior
.
-type context_attribs_list() :: [
    {context_major_version, pos_integer()} |
    {context_minor_version, pos_integer()} |
    {context_opengl_profile_mask, [
        context_opengl_core_profile_bit | context_opengl_compatibility_profile_bit
    ]} |
    {context_opengl_debug, boolean()} |
    {context_opengl_forward_compatible, boolean()} |
    {context_opengl_robust_access, boolean()} |
    {context_opengl_reset_notification_strategy,
        no_reset_notification | lose_context_on_reset
    }
].
-type context_attrib_get() ::
    config_id |
    context_client_type |
    context_client_version |
    render_buffer
.

init() ->
    % XXX: Generated library should be `egl.so` but erlang.mk won't allow that.
    ok = erlang:load_nif("./priv/egl_1_5", 0).

%%
%% eglChooseConfig — return a list of EGL frame buffer configurations that match specified attributes
%%
%% - foo
%% - bar
%%
-spec choose_config(display(), config_attribs_list()) ->
    {ok, [config()]} | not_ok.
choose_config(Display, AttribsList) ->
    % We transform the list of attributes into a list of integers so the NIF
    % implementation is easier to write.
    AttribsListRaw = lists:foldl(fun
        ({alpha_mask_size, AlphaMaskSize}, Accumulator) ->
            Accumulator ++ [?EGL_ALPHA_MASK_SIZE, AlphaMaskSize];
        ({alpha_size, AlphaSize}, Accumulator) ->
            Accumulator ++ [?EGL_ALPHA_SIZE, AlphaSize];
        ({bind_to_texture_rgb, BindToTextureRgb}, Accumulator) ->
            Value = case BindToTextureRgb of
                dont_care -> ?EGL_DONT_CARE;
                BindToTextureRgb_ -> BindToTextureRgb_
            end,
            Accumulator ++ [?EGL_BIND_TO_TEXTURE_RGB, Value];
        ({bind_to_texture_rgba, BindToTextureRgba}, Accumulator) ->
            Value = case BindToTextureRgba of
                dont_care -> ?EGL_DONT_CARE;
                BindToTextureRgba_ -> BindToTextureRgba_
            end,
            Accumulator ++ [?EGL_BIND_TO_TEXTURE_RGBA, Value];
        ({blue_size, BlueSize}, Accumulator) ->
            Accumulator ++ [?EGL_BLUE_SIZE, BlueSize];
        ({buffer_size, BufferSize}, Accumulator) ->
            Accumulator ++ [?EGL_BUFFER_SIZE, BufferSize];

        ({color_buffer_type, ColorBufferType}, Accumulator) ->
            Value = case ColorBufferType of
                rgb_buffer -> ?EGL_RGB_BUFFER;
                luminance_buffer -> ?EGL_LUMINANCE_BUFFER
            end,
            Accumulator ++ [?EGL_COLOR_BUFFER_TYPE, Value];
        ({config_caveat, ConfigCaveat}, Accumulator) ->
            Value = case ConfigCaveat of
                dont_care -> ?EGL_DONT_CARE;
                none -> ?EGL_NONE;
                slow_config -> ?EGL_SLOW_CONFIG
            end,
            Accumulator ++ [?EGL_CONFIG_CAVEAT, Value];
        ({config_id, ConfigId}, Accumulator) ->
            Accumulator ++ [?EGL_CONFIG_ID, ConfigId];
        ({conformant, Conformant}, Accumulator) ->
            Flags = lists:map(fun
                (opengl_bit) -> ?EGL_OPENGL_BIT;
                (opengl_es_bit) -> ?EGL_OPENGL_ES_BIT;
                (opengl_es2_bit) -> ?EGL_OPENGL_ES2_BIT;
                (openvg_bit) -> ?EGL_OPENVG_BIT
            end, Conformant),
            Value = lists:foldl(fun(Flag, AccumulatorBis) ->  Flag bor AccumulatorBis end, 0, Flags),
            Accumulator ++ [?EGL_CONFORMANT, Value];
        ({depth_size, DepthSize}, Accumulator) ->
            Accumulator ++ [?EGL_DEPTH_SIZE, DepthSize];
        ({green_size, GreenSize}, Accumulator) ->
            Accumulator ++ [?EGL_GREEN_SIZE, GreenSize];
        ({level, Level}, Accumulator) ->
            Accumulator ++ [?EGL_LEVEL, Level];
        ({luminance_size, LuminanceSize}, Accumulator) ->
            Accumulator ++ [?EGL_LUMINANCE_SIZE, LuminanceSize];
        ({native_renderable, NativeRenderable}, Accumulator) ->
            Value = case NativeRenderable of
                dont_care -> ?EGL_DONT_CARE;
                NativeRenderable_ -> NativeRenderable_
            end,
            Accumulator ++ [?EGL_NATIVE_RENDERABLE, Value];
        ({max_swap_interval, MaxSwapInterval}, Accumulator) ->
            Value = case MaxSwapInterval of
                dont_care -> ?EGL_DONT_CARE;
                MaxSwapInterval_ -> MaxSwapInterval_
            end,
            Accumulator ++ [?EGL_MAX_SWAP_INTERVAL, Value];
        ({min_swap_interval, MinSwapInterval}, Accumulator) ->
            Value = case MinSwapInterval of
                dont_care -> ?EGL_DONT_CARE;
                MinSwapInterval_ -> MinSwapInterval_
            end,
            Accumulator ++ [?EGL_MIN_SWAP_INTERVAL, Value];

        ({red_size, RedSize}, Accumulator) ->
            Accumulator ++ [?EGL_RED_SIZE, RedSize];
        ({sample_buffers, SampleBuffers}, Accumulator) ->
            Accumulator ++ [?EGL_SAMPLE_BUFFERS, SampleBuffers];
        ({samples, Samples}, Accumulator) ->
            Accumulator ++ [?EGL_SAMPLES, Samples];
        ({stencil_size, StencilSize}, Accumulator) ->
            Accumulator ++ [?EGL_STENCIL_SIZE, StencilSize];
        ({renderable_type, RenderableType}, Accumulator) ->
            Flags = lists:map(fun
                (opengl_bit) -> ?EGL_OPENGL_BIT;
                (opengl_es_bit) -> ?EGL_OPENGL_ES_BIT;
                (opengl_es2_bit) -> ?EGL_OPENGL_ES2_BIT;
                (openvg_bit) -> ?EGL_OPENVG_BIT
            end, RenderableType),
            Value = lists:foldl(fun(Flag, AccumulatorBis) ->  Flag bor AccumulatorBis end, 0, Flags),
            Accumulator ++ [?EGL_RENDERABLE_TYPE, Value];
        ({surface_type, SurfaceType}, Accumulator) ->
            Flags = lists:map(fun
                (multisample_resolve_box_bit) -> ?EGL_MULTISAMPLE_RESOLVE_BOX_BIT;
                (pbuffer_bit) -> ?EGL_PBUFFER_BIT;
                (pixmat_bit) -> ?EGL_PIXMAP_BIT;
                (swap_behavior_preserved_bit) -> ?EGL_SWAP_BEHAVIOR_PRESERVED_BIT;
                (vg_alpha_format_pre_bit) -> ?EGL_VG_ALPHA_FORMAT_PRE_BIT;
                (vg_colorspace_linear_bit) -> ?EGL_VG_COLORSPACE_LINEAR_BIT;
                (window_bit) -> ?EGL_WINDOW_BIT
            end, SurfaceType),
            Value = lists:foldl(fun(Flag, AccumulatorBis) ->  Flag bor AccumulatorBis end, 0, Flags),
            Accumulator ++ [?EGL_SURFACE_TYPE, Value];
        ({transparent_type, TransparentType}, Accumulator) ->
            Value = case TransparentType of
                none -> ?EGL_NONE;
                transparent_rgb -> ?EGL_TRANSPARENT_RGB
            end,
            Accumulator ++ [?EGL_TRANSPARENT_TYPE, Value];
        ({transparent_red_value, TransparentRedValue}, Accumulator) ->
            Value = case TransparentRedValue of
                dont_care -> ?EGL_DONT_CARE;
                TransparentRedValue_ -> TransparentRedValue_
            end,
            Accumulator ++ [?EGL_TRANSPARENT_RED_VALUE, Value];
        ({transparent_green_value, TransparentGreenValue}, Accumulator) ->
            Value = case TransparentGreenValue of
                dont_care -> ?EGL_DONT_CARE;
                TransparentGreenValue_ -> TransparentGreenValue_
            end,
            Accumulator ++ [?EGL_TRANSPARENT_GREEN_VALUE, Value];
        ({transparent_blue_value, TransparentBlueValue}, Accumulator) ->
            Value = case TransparentBlueValue of
                dont_care -> ?EGL_DONT_CARE;
                TransparentBlueValue_ -> TransparentBlueValue_
            end,
            Accumulator ++ [?EGL_TRANSPARENT_BLUE_VALUE, Value]
    end, [], AttribsList),
    choose_config_raw(Display, AttribsListRaw).

choose_config_raw(_Display, _AttribsList) ->
    erlang:nif_error(nif_library_not_loaded).

copy_buffers(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglCreateContext — create a new EGL rendering context
%%
%% - foo
%% - bar
%%
-spec create_context(display(), config(), no_context | context(), context_attribs_list()) ->
    {ok, context()} | not_ok.
create_context(Display, Config, ShareContext, AttribsList) ->
    % We transform the list of attributes into a list of integers so the NIF
    % implementation is easier to write.
    AttribsListRaw = lists:foldl(fun
        ({context_major_version, ContextMajorVersion}, Accumulator) ->
            Accumulator ++ [?EGL_CONTEXT_MAJOR_VERSION, ContextMajorVersion];
        ({context_minor_version, ContextMinorVersion}, Accumulator) ->
            Accumulator ++ [?EGL_CONTEXT_MINOR_VERSION, ContextMinorVersion];
        % ({context_opengl_profile_mask, ContextOpenGLProfileMask}, Accumulator) ->
        %     Flags = lists:map(fun
        %         (context_opengl_core_profile_bit) -> ?EGL_CONTEXT_OPENGL_CORE_PROFILE_BIT;
        %         (context_opengl_compatibility_profile_bit) -> ?EGL_CONTEXT_OPENGL_COMPATIBILITY_PROFILE_BIT
        %     end, ContextOpenGLProfileMask),
        %     Value = lists:foldl(fun(Flag, AccumulatorBis) ->  Flag bor AccumulatorBis end, 0, Flags),
        %     Accumulator ++ [?EGL_CONTEXT_OPENGL_PROFILE_MASK, Value];
        ({context_opengl_debug, ContextOpenGLDebug}, Accumulator) ->
            Value = case ContextOpenGLDebug of
                true -> ?EGL_TRUE;
                false -> ?EGL_FALSE
            end,
            Accumulator ++ [?EGL_CONTEXT_OPENGL_DEBUG, Value];
        ({context_opengl_forward_compatible, ContextOpenGLForwardCompatible}, Accumulator) ->
            Value = case ContextOpenGLForwardCompatible of
                true -> ?EGL_TRUE;
                false -> ?EGL_FALSE
            end,
            Accumulator ++ [?EGL_CONTEXT_OPENGL_FORWARD_COMPATIBLE, Value];
        ({context_opengl_robust_access, ContextOpenGLRobustAccess}, Accumulator) ->
            Value = case ContextOpenGLRobustAccess of
                true -> ?EGL_TRUE;
                false -> ?EGL_FALSE
            end,
            Accumulator ++ [?EGL_CONTEXT_OPENGL_ROBUST_ACCESS, Value];
        ({context_opengl_reset_notification_strategy, ContextOpenGLResetNotificationStrategy}, Accumulator) ->
            Value = case ContextOpenGLResetNotificationStrategy of
                no_reset_notification -> ?EGL_NO_RESET_NOTIFICATION;
                lose_context_on_reset -> ?EGL_LOSE_CONTEXT_ON_RESET
            end,
            Accumulator ++ [?EGL_CONTEXT_OPENGL_RESET_NOTIFICATION_STRATEGY, Value]
    end, [], AttribsList),
    create_context_raw(Display, Config, ShareContext, AttribsListRaw).

create_context_raw(_Display, _Config, _ShareContext, _AttribsList) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglCreatePbufferSurface — create a new EGL pixel buffer surface
%%
%% - foo
%% - bar
%%
-spec create_pbuffer_surface(display(), config(), surface_attribs_list()) ->
    {ok, surface()} | not_ok.
create_pbuffer_surface(Display, Config, AttribsList) ->
    % We transform the list of attributes into a list of integers so the NIF
    % implementation is easier to write.
    AttribsListRaw = lists:foldl(fun
        ({gl_colorspace, GlColorspace}, Accumulator) ->
            Value = case GlColorspace of
                gl_colorspace_srgb -> ?EGL_GL_COLORSPACE_SRGB;
                gl_colorspace_linear -> ?EGL_GL_COLORSPACE_LINEAR
            end,
            Accumulator ++ [?EGL_GL_COLORSPACE, Value];
        ({height, Height}, Accumulator) ->
            Accumulator ++ [?EGL_HEIGHT, Height];
        ({largest_pbuffer, LargestPbuffer}, Accumulator) ->
            Value = case LargestPbuffer of
                true -> ?EGL_TRUE;
                false -> ?EGL_FALSE
            end,
            Accumulator ++ [?EGL_LARGEST_PBUFFER, Value];
        ({mipmap_texture, MipmapTexture}, Accumulator) ->
            Value = case MipmapTexture of
                true -> ?EGL_TRUE;
                false -> ?EGL_FALSE
            end,
            Accumulator ++ [?EGL_MIPMAP_TEXTURE, Value];
        ({texture_format, TextureFormat}, Accumulator) ->
            Value = case TextureFormat of
                no_texture -> ?EGL_NO_TEXTURE;
                texture_rgb -> ?EGL_TEXTURE_RGB;
                texture_rgba -> ?EGL_TEXTURE_RGBA
            end,
            Accumulator ++ [?EGL_TEXTURE_FORMAT, Value];
        ({texture_target, TextureTarget}, Accumulator) ->
            Value = case TextureTarget of
                no_texture -> ?EGL_NO_TEXTURE;
                texture_2d -> ?EGL_TEXTURE_2D
            end,
            Accumulator ++ [?EGL_TEXTURE_TARGET, Value];
        ({vg_alpha_format, VgAlphaFormat}, Accumulator) ->
            Value = case VgAlphaFormat of
                vg_alpha_format_nonpre -> ?EGL_VG_ALPHA_FORMAT_NONPRE;
                vg_alpha_format_pre -> ?EGL_VG_ALPHA_FORMAT_PRE
            end,
            Accumulator ++ [?EGL_VG_ALPHA_FORMAT, Value];
        ({vg_colorspace, VgColorspace}, Accumulator) ->
            Value = case VgColorspace of
                vg_colorspace_srgb -> ?EGL_VG_COLORSPACE_sRGB;
                vg_colorspace_linear -> ?EGL_VG_COLORSPACE_LINEAR
            end,
            Accumulator ++ [?EGL_VG_COLORSPACE, Value];
        ({width, Width}, Accumulator) ->
            Accumulator ++ [?EGL_WIDTH, Width]
    end, [], AttribsList),
    create_pbuffer_surface_raw(Display, Config, AttribsListRaw).

create_pbuffer_surface_raw(_Display, _Config, _AttribsList) ->
    erlang:nif_error(nif_library_not_loaded).

create_pixmap_surface(_A, _B, _C, _D) ->
    erlang:nif_error(nif_library_not_loaded).

create_window_surface(_A, _B, _C, _D) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglDestroyContext — destroy an EGL rendering context
%%
%% - foo
%% - bar
%%
-spec destroy_context(display(), context()) -> ok | not_ok.
destroy_context(_Display, _Context) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglDestroySurface — destroy an EGL surface
%%
%% - foo
%% - bar
%%
-spec destroy_surface(display(), surface()) -> ok | not_ok.
destroy_surface(_Dislay, _Surface) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetConfigAttrib — return information about an EGL frame buffer configuration
%%
%% - foo
%% - bar
%%
-spec get_config_attrib(display(), config(), config_attrib()) ->
    {ok, term()} | not_ok.
get_config_attrib(Display, Config, Attribute) ->
    % We transform the attribute into an integer so the NIF implementation is
    % easier to write. Also, the NIF implementation purposely returns the raw
    % integer value so we can transform it here instead.
    AttributeRaw = case Attribute of
        alpha_size -> ?EGL_ALPHA_SIZE;
        alpha_mask_size -> ?EGL_ALPHA_MASK_SIZE;
        bind_to_texture_rgb -> ?EGL_BIND_TO_TEXTURE_RGB;
        bind_to_texture_rgba -> ?EGL_BIND_TO_TEXTURE_RGBA;
        blue_size -> ?EGL_BLUE_SIZE;
        buffer_size -> ?EGL_BUFFER_SIZE;
        color_buffer_type -> ?EGL_COLOR_BUFFER_TYPE;
        config_caveat -> ?EGL_CONFIG_CAVEAT;
        config_id -> ?EGL_CONFIG_ID;
        conformant -> ?EGL_CONFORMANT;
        depth_size -> ?EGL_DEPTH_SIZE;
        green_size -> ?EGL_GREEN_SIZE;
        level -> ?EGL_LEVEL;
        luminance_size -> ?EGL_LUMINANCE_SIZE;
        max_pbuffer_width -> ?EGL_MAX_PBUFFER_WIDTH;
        max_pbuffer_height -> ?EGL_MAX_PBUFFER_HEIGHT;
        max_pbuffer_pixels -> ?EGL_MAX_PBUFFER_PIXELS;
        max_swap_interval -> ?EGL_MAX_SWAP_INTERVAL;
        min_swap_interval -> ?EGL_MIN_SWAP_INTERVAL;
        native_renderable -> ?EGL_NATIVE_RENDERABLE;
        native_visual_id -> ?EGL_NATIVE_VISUAL_ID;
        native_visual_type -> ?EGL_NATIVE_VISUAL_TYPE;
        red_size -> ?EGL_RED_SIZE;
        renderable_type -> ?EGL_RENDERABLE_TYPE;
        sample_buffers -> ?EGL_SAMPLE_BUFFERS;
        samples -> ?EGL_SAMPLES;
        stencil_size -> ?EGL_STENCIL_SIZE;
        surface_type -> ?EGL_SURFACE_TYPE;
        transparent_type -> ?EGL_TRANSPARENT_TYPE;
        transparent_red_value -> ?EGL_TRANSPARENT_RED_VALUE;
        transparent_green_value -> ?EGL_TRANSPARENT_GREEN_VALUE;
        transparent_blue_value -> ?EGL_TRANSPARENT_BLUE_VALUE
    end,
    case get_config_attrib_raw(Display, Config, AttributeRaw) of
        {ok, ValueRaw} ->
            Value = case Attribute of
                alpha_size ->
                    ValueRaw;
                alpha_mask_size ->
                    ValueRaw;
                bind_to_texture_rgb ->
                    case ValueRaw of
                        ?EGL_TRUE -> true;
                        ?EGL_FALSE -> false
                    end;
                bind_to_texture_rgba ->
                    case ValueRaw of
                        ?EGL_TRUE -> true;
                        ?EGL_FALSE -> false
                    end;
                blue_size ->
                    ValueRaw;
                buffer_size ->
                    ValueRaw;
                color_buffer_type ->
                    case ValueRaw of
                        ?EGL_RGB_BUFFER -> rgb_buffer;
                        ?EGL_LUMINANCE_BUFFER -> luminance_buffer
                    end;
                config_caveat ->
                    case ValueRaw of
                        ?EGL_NONE -> none;
                        ?EGL_SLOW_CONFIG -> slow_config
                    end;
                config_id ->
                    ValueRaw;
                conformant ->
                    to_bitmask(ValueRaw, #{
                        ?EGL_OPENGL_BIT => opengl_bit,
                        ?EGL_OPENGL_ES_BIT => opengl_es_bit,
                        ?EGL_OPENGL_ES2_BIT => opengl_es2_bit,
                        ?EGL_OPENVG_BIT => openvg_bit
                    });
                depth_size ->
                    ValueRaw;
                green_size ->
                    ValueRaw;
                level ->
                    ValueRaw;
                luminance_size ->
                    ValueRaw;
                max_pbuffer_width ->
                    ValueRaw;
                max_pbuffer_height ->
                    ValueRaw;
                max_pbuffer_pixels ->
                    ValueRaw;
                max_swap_interval ->
                    ValueRaw;
                min_swap_interval ->
                    ValueRaw;
                native_renderable ->
                    case ValueRaw of
                        ?EGL_TRUE -> true;
                        ?EGL_FALSE -> false
                    end;
                native_visual_id ->
                    ValueRaw;
                native_visual_type ->
                    ValueRaw;
                red_size ->
                    ValueRaw;
                renderable_type ->
                    to_bitmask(ValueRaw, #{
                        ?EGL_OPENGL_BIT => opengl_bit,
                        ?EGL_OPENGL_ES_BIT => opengl_es_bit,
                        ?EGL_OPENGL_ES2_BIT => opengl_es2_bit,
                        ?EGL_OPENVG_BIT => openvg_bit
                    });
                sample_buffers ->
                    ValueRaw;
                samples ->
                    ValueRaw;
                stencil_size ->
                    ValueRaw;
                surface_type ->
                    to_bitmask(ValueRaw, #{
                        ?EGL_MULTISAMPLE_RESOLVE_BOX_BIT => multisample_resolve_box_bit,
                        ?EGL_PBUFFER_BIT => pbuffer_bit,
                        ?EGL_PIXMAP_BIT => pixmat_bit,
                        ?EGL_SWAP_BEHAVIOR_PRESERVED_BIT => swap_behavior_preserved_bit,
                        ?EGL_VG_ALPHA_FORMAT_PRE_BIT => vg_alpha_format_pre_bit,
                        ?EGL_VG_COLORSPACE_LINEAR_BIT => vg_colorspace_linear_bit,
                        ?EGL_WINDOW_BIT => window_bit
                    });
                transparent_type ->
                    case ValueRaw of
                        ?EGL_NONE -> none;
                        ?EGL_TRANSPARENT_RGB -> transparent_rgb
                    end;
                transparent_red_value ->
                    ValueRaw;
                transparent_green_value ->
                    ValueRaw;
                transparent_blue_value ->
                    ValueRaw
            end,
            {ok, Value};
        not_ok ->
            not_ok
    end.

get_config_attrib_raw(_Display, _Config, _Attribute) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetConfigs — return a list of all EGL frame buffer configurations for a display
%%
%% - foo
%% - bar
%%
-spec get_configs(display()) -> {ok, [config()]} | not_ok.
get_configs(_Display) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetCurrentDisplay — return the display for the current EGL rendering context
%%
%% - foo
%% - bar
%%
-spec get_current_display() -> no_display | display().
get_current_display() ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetCurrentSurface — return the read or draw surface for the current EGL rendering context
%%
%% - foo
%% - bar
%%
-spec get_current_surface(read | draw) -> no_surface | surface().
get_current_surface(ReadDraw) ->
    ReadDrawRaw = case ReadDraw of
        read -> ?EGL_READ;
        draw -> ?EGL_DRAW
    end,
    get_current_surface_raw(ReadDrawRaw).

get_current_surface_raw(_ReadDraw) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetDisplay — return an EGL display connection
%%
%% - Parameter must be 'default_display'.
%%
%% XXX: First parameter must be reworked.
%%
-spec get_display(default_display) -> no_display | display().
get_display(_NativeDisplay) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetError — return error information
%%
%% - foo
%% - bar
%%
-spec get_error() ->
    success |
    not_initialized |
    bad_access |
    bad_alloc |
    bad_attribute |
    bad_context |
    bad_config |
    bad_current_surface |
    bad_display |
    bad_surface |
    bad_match |
    bad_parameter |
    bad_native_pixmap |
    bad_native_window |
    context_lost.
get_error() ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglInitialize — initialize an EGL display connection
%%
%% - Unlike original C function, it returns EGL version.
%% - bar
%%
-spec initialize(display()) -> {ok, {pos_integer(), pos_integer()}} | not_ok.
initialize(_Display) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglMakeCurrent — attach an EGL rendering context to EGL surfaces
%%
%% - foo
%% - bar
%%
-spec make_current(
    display(),
    no_surface | surface(),
    no_surface | surface(),
    no_context | context()
) -> ok | not_ok.
make_current(_Display, _Draw, _Read, _Context) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglQueryContext — return EGL rendering context information
%%
%% - foo
%% - bar
%%
-spec query_context(display(), context(), context_attrib_get()) ->
    {ok, term()} | not_ok.
query_context(Display, Context, Attribute) ->
    AttributeRaw = case Attribute of
        config_id -> ?EGL_CONFIG_ID;
        context_client_type -> ?EGL_CONTEXT_CLIENT_TYPE;
        context_client_version -> ?EGL_CONTEXT_CLIENT_VERSION;
        render_buffer -> ?EGL_RENDER_BUFFER
    end,
    case query_context_raw(Display, Context, AttributeRaw) of
        {ok, ValueRaw} ->
            Value = case Attribute of
                config_id ->
                    ValueRaw;
                context_client_type ->
                    case ValueRaw of
                        ?EGL_OPENGL_API -> opengl_api;
                        ?EGL_OPENGL_ES_API -> opengl_es_api;
                        ?EGL_OPENVG_API -> openvg_api
                    end;
                context_client_version ->
                    % XXX: Double-check this.
                    ValueRaw;
                render_buffer ->
                    case ValueRaw of
                        ?EGL_BACK_BUFFER -> back_buffer;
                        ?EGL_SINGLE_BUFFER -> single_buffer;
                        ?EGL_NONE -> none
                    end
            end,
            {ok, Value};
        not_ok ->
            not_ok
    end.

query_context_raw(_Display, _Context, _Attribute) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglQueryString — return a string describing properties of the EGL client or of an EGL display connection
%%
%% - foo
%% - bar
%%
-spec query_string(
    no_display | display(),
    client_apis | vendor | version | extensions
) -> {ok, string()} | not_ok.
query_string(_Display, _Name) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglQuerySurface — return EGL surface information
%%
%% - foo
%% - bar
%%
-spec query_surface(display(), surface(), surface_attrib_get()) ->
    {ok, term()} | not_ok.
query_surface(Display, Surface, Attribute) ->
    AttributeRaw = case Attribute of
        config_id -> ?EGL_CONFIG_ID;
        gl_colorspace -> ?EGL_GL_COLORSPACE;
        height -> ?EGL_HEIGHT;
        horizontal_resolution -> ?EGL_HORIZONTAL_RESOLUTION;
        largest_pbuffer -> ?EGL_LARGEST_PBUFFER;
        mipmap_level -> ?EGL_MIPMAP_LEVEL;
        mipmap_texture -> ?EGL_MIPMAP_TEXTURE;
        multisample_resolve -> ?EGL_MULTISAMPLE_RESOLVE;
        pixel_aspect_ratio -> ?EGL_PIXEL_ASPECT_RATIO;
        render_buffer -> ?EGL_RENDER_BUFFER;
        swap_behavior -> ?EGL_SWAP_BEHAVIOR;
        texture_format -> ?EGL_TEXTURE_FORMAT;
        texture_target -> ?EGL_TEXTURE_TARGET;
        vertical_resolution -> ?EGL_VERTICAL_RESOLUTION;
        vg_alpha_format -> ?EGL_VG_ALPHA_FORMAT;
        vg_colorspace -> ?EGL_VG_COLORSPACE;
        width -> ?EGL_WIDTH
    end,
    case query_surface_raw(Display, Surface, AttributeRaw) of
        {ok, ValueRaw} ->
            Value = case Attribute of
                config_id ->
                    ValueRaw;
                gl_colorspace ->
                    case ValueRaw of
                        ?EGL_GL_COLORSPACE_SRGB -> gl_colorspace_srgb;
                        ?EGL_GL_COLORSPACE_LINEAR -> gl_colorspace_linear
                    end;
                height ->
                    ValueRaw;
                horizontal_resolution ->
                    ValueRaw;
                largest_pbuffer ->
                    ValueRaw;
                mipmap_level ->
                    ValueRaw;
                mipmap_texture ->
                    case ValueRaw of
                        ?EGL_TRUE -> true;
                        ?EGL_FALSE -> false
                    end;
                multisample_resolve ->
                    case ValueRaw of
                        ?EGL_MULTISAMPLE_RESOLVE_DEFAULT -> multisample_resolve_default;
                        ?EGL_MULTISAMPLE_RESOLVE_BOX -> multisample_resolve_box
                    end;
                pixel_aspect_ratio ->
                    ValueRaw;
                render_buffer ->
                    % XXX: Verify if those are the only two legit values.
                    case ValueRaw of
                        ?EGL_BACK_BUFFER -> back_buffer;
                        ?EGL_SINGLE_BUFFER -> single_buffer
                    end;
                swap_behavior ->
                    case ValueRaw of
                        ?EGL_BUFFER_PRESERVED -> buffer_preserved;
                        ?EGL_BUFFER_DESTROYED -> buffer_destroyed
                    end;
                texture_format ->
                    case ValueRaw of
                        ?EGL_NO_TEXTURE -> no_texture;
                        ?EGL_TEXTURE_RGB -> texture_rgb;
                        ?EGL_TEXTURE_RGBA -> texture_rgba
                    end;
                texture_target ->
                    case ValueRaw of
                        ?EGL_NO_TEXTURE -> no_texture;
                        ?EGL_TEXTURE_2D -> texture_2d
                    end;
                vertical_resolution ->
                    ValueRaw;
                vg_alpha_format ->
                    case ValueRaw of
                        ?EGL_VG_ALPHA_FORMAT_NONPRE -> vg_alpha_format_nonpre;
                        ?EGL_VG_ALPHA_FORMAT_PRE -> vg_alpha_format_pre
                    end;
                vg_colorspace ->
                    case ValueRaw of
                        ?EGL_VG_COLORSPACE_sRGB -> vg_colorspace_srgb;
                        ?EGL_VG_COLORSPACE_LINEAR -> vg_colorspace_linear
                    end;
                width ->
                    ValueRaw
            end,
            {ok, Value};
        not_ok ->
            not_ok
    end.

query_surface_raw(_Display, _Surface, _Attribute) ->
    erlang:nif_error(nif_library_not_loaded).

swap_buffers(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglTerminate — terminate an EGL display connection
%%
%% - foo.
%% - bar
%%
-spec terminate(display()) -> ok | not_ok.
terminate(_A) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglWaitGL — Complete GL execution prior to subsequent native rendering calls
%%
%% - foo
%% - bar
%%
-spec wait_gl() -> ok | not_ok.
wait_gl() ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglWaitNative — complete native execution prior to subsequent GL rendering calls
%%
%% - foo
%% - bar
%%
-spec wait_native(core_native_engine) -> ok | not_ok.
wait_native(_Engine) ->
    erlang:nif_error(nif_library_not_loaded).

bind_tex_image(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

release_tex_image(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglSurfaceAttrib — set an EGL surface attribute
%%
%% - foo
%% - bar
%%
-spec surface_attrib(display(), surface(), surface_attrib_set(), term()) -> ok | not_ok.
surface_attrib(Display, Surface, Attribute, Value) ->
    {RawAttribute, RawValue} = case
        Attribute of
            mipmap_level ->
                {?EGL_MIPMAP_LEVEL, Value};
            multisample_resolve ->
                RawValue_ = case Value of
                    multisample_resolve_default -> ?EGL_MULTISAMPLE_RESOLVE_DEFAULT;
                    multisample_resolve_box -> ?EGL_MULTISAMPLE_RESOLVE_BOX
                end,
                {?EGL_MULTISAMPLE_RESOLVE, RawValue_};
            swap_behavior ->
                RawValue_ = case Value of
                    buffer_preserved -> ?EGL_BUFFER_PRESERVED;
                    buffer_destroyed -> ?EGL_BUFFER_DESTROYED
                end,
                {?EGL_SWAP_BEHAVIOR, RawValue_}
    end,
    surface_attrib_raw(Display, Surface, RawAttribute, RawValue).

surface_attrib_raw(_Display, _Surface, _Attribute, _Value) ->
    erlang:nif_error(nif_library_not_loaded).

swap_interval(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglBindAPI — Set the current rendering API
%%
%% - foo
%% - bar
%%
-spec bind_api(opengl_api | opengl_es_api | openvg_api) -> ok | not_ok.
bind_api(Api) ->
    ApiRaw = case Api of
        opengl_api -> ?EGL_OPENGL_API;
        opengl_es_api -> ?EGL_OPENGL_ES_API;
        openvg_api -> ?EGL_OPENVG_API
    end,
    bind_api_raw(ApiRaw).

bind_api_raw(_Api) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglQueryAPI — Query the current rendering API
%%
%% - foo
%% - bar
%%
-spec query_api() -> opengl_api | opengl_es_api | openvg_api | none.
query_api() ->
    ApiRaw = query_api_raw(),
    case ApiRaw of
        ?EGL_OPENGL_API -> opengl_api;
        ?EGL_OPENGL_ES_API -> opengl_es_api;
        ?EGL_OPENVG_API -> openvg_api;
        ?EGL_NONE -> none
    end.

query_api_raw() ->
    erlang:nif_error(nif_library_not_loaded).

create_pbuffer_from_client_buffer(_A, _B, _C, _D, _E) ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglReleaseThread — Release EGL per-thread state
%%
%% - foo
%% - bar
%%
-spec release_thread() -> ok | not_ok.
release_thread() ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglWaitClient — Complete client API execution prior to subsequent native rendering calls
%%
%% - foo
%% - bar
%%
-spec wait_client() -> ok | not_ok.
wait_client() ->
    erlang:nif_error(nif_library_not_loaded).

%%
%% eglGetCurrentContext — return the current EGL rendering context
%%
%% - foo
%% - bar
%%
-spec get_current_context() -> no_context | context().
get_current_context() ->
    erlang:nif_error(nif_library_not_loaded).

create_sync(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

destroy_sync(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

client_wait_sync(_A, _B, _C, _D) ->
    erlang:nif_error(nif_library_not_loaded).

get_sync_attrib(_A, _B, _C, _D) ->
    erlang:nif_error(nif_library_not_loaded).

create_image(_A, _B, _C, _D, _E) ->
    erlang:nif_error(nif_library_not_loaded).

destroy_image(_A, _B) ->
    erlang:nif_error(nif_library_not_loaded).

get_platform_display(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

create_platform_window_surface(_A, _B, _C, _D) ->
    erlang:nif_error(nif_library_not_loaded).

create_platform_pixmap_surface(_A, _B, _C, _D) ->
    erlang:nif_error(nif_library_not_loaded).

wait_sync(_A, _B, _C) ->
    erlang:nif_error(nif_library_not_loaded).

to_bitmask(Value, Flags) ->
    maps:fold(fun(Left, Right, Accumulator) ->
        case (Value band Left) =/= 0 of
            true ->
                [Right | Accumulator];
            false ->
                Accumulator
        end
    end, [], Flags).
