-module(egl_get_config_attrib_test).
-include_lib("eunit/include/eunit.hrl").

assert_is_enum(Value, Elements) ->
    ?assert(lists:member(Value, Elements)).

assert_is_bitmask(Value, Flags) when is_list(Value) ->
    RemainingValues = lists:foldl(fun(Flag, Accumulator) ->
        ?assert(lists:member(Flag, Flags)),
        lists:delete(Flag, Accumulator)
    end, Value, Flags),
    ?assert(RemainingValues =:= []).

egl_get_config_attrib_test() ->
    Display = egl:get_display(default_display),
    {ok, _Version} = egl:initialize(Display),

    {ok, Configs} = egl:get_configs(Display),
    lists:foreach(fun(Config) ->
        {ok, AlphaSize} = egl:get_config_attrib(Display, Config, alpha_size),
        {ok, AlphaMaskSize} = egl:get_config_attrib(Display, Config, alpha_mask_size),
        {ok, BindToTextureRgb} = egl:get_config_attrib(Display, Config, bind_to_texture_rgb),
        {ok, BindToTextureRgba} = egl:get_config_attrib(Display, Config, bind_to_texture_rgba),
        {ok, BlueSize} = egl:get_config_attrib(Display, Config, blue_size),
        {ok, BufferSize} = egl:get_config_attrib(Display, Config, buffer_size),
        {ok, ColorBufferType} = egl:get_config_attrib(Display, Config, color_buffer_type),
        {ok, ConfigCaveat} = egl:get_config_attrib(Display, Config, config_caveat),
        {ok, ConfigId} = egl:get_config_attrib(Display, Config, config_id),
        {ok, Conformant} = egl:get_config_attrib(Display, Config, conformant),
        {ok, DepthSize} = egl:get_config_attrib(Display, Config, depth_size),
        {ok, GreenSize} = egl:get_config_attrib(Display, Config, green_size),
        {ok, Level} = egl:get_config_attrib(Display, Config, level),
        {ok, LuminanceSize} = egl:get_config_attrib(Display, Config, luminance_size),
        {ok, MaxPbufferWidth} = egl:get_config_attrib(Display, Config, max_pbuffer_width),
        {ok, MaxPbufferHeight} = egl:get_config_attrib(Display, Config, max_pbuffer_height),
        {ok, MaxPbufferPixels} = egl:get_config_attrib(Display, Config, max_pbuffer_pixels),
        {ok, MaxSwapInterval} = egl:get_config_attrib(Display, Config, max_swap_interval),
        {ok, MinSwapInterval} = egl:get_config_attrib(Display, Config, min_swap_interval),
        {ok, NativeRenderable} = egl:get_config_attrib(Display, Config, native_renderable),
        {ok, NativeVisualId} = egl:get_config_attrib(Display, Config, native_visual_id),
        {ok, NativeVisualType} = egl:get_config_attrib(Display, Config, native_visual_type),
        {ok, RedSize} = egl:get_config_attrib(Display, Config, red_size),
        {ok, RenderableType} = egl:get_config_attrib(Display, Config, renderable_type),
        {ok, SampleBuffers} = egl:get_config_attrib(Display, Config, sample_buffers),
        {ok, Samples} = egl:get_config_attrib(Display, Config, samples),
        {ok, StencilSize} = egl:get_config_attrib(Display, Config, stencil_size),
        {ok, SurfaceType} = egl:get_config_attrib(Display, Config, surface_type),
        {ok, TransparentType} = egl:get_config_attrib(Display, Config, transparent_type),
        {ok, TransparentRedValue} = egl:get_config_attrib(Display, Config, transparent_red_value),
        {ok, TransparentGreenValue} = egl:get_config_attrib(Display, Config, transparent_green_value),
        {ok, TransparentBlueValue} = egl:get_config_attrib(Display, Config, transparent_blue_value),

        ?assert(is_integer(AlphaSize)),
        ?assert(is_integer(AlphaMaskSize)),
        ?assert(is_boolean(BindToTextureRgb)),
        ?assert(is_boolean(BindToTextureRgba)),
        ?assert(is_integer(BlueSize)),
        ?assert(is_integer(BufferSize)),
        assert_is_enum(ColorBufferType, [rgb_buffer, luminance_buffer]),
        assert_is_enum(ConfigCaveat, [none, slow_config]),
        ?assert(is_integer(ConfigId)),
        assert_is_bitmask(
            Conformant,
            [opengl_bit, opengl_es_bit, opengl_es2_bit, openvg_bit]
        ),
        ?assert(is_integer(DepthSize)),
        ?assert(is_integer(GreenSize)),
        ?assert(is_integer(Level)),
        ?assert(is_integer(LuminanceSize)),
        ?assert(is_integer(MaxPbufferWidth)),
        ?assert(is_integer(MaxPbufferHeight)),
        ?assert(is_integer(MaxPbufferPixels)),
        ?assert(is_integer(MaxSwapInterval)),
        ?assert(is_integer(MinSwapInterval)),
        ?assert(is_boolean(NativeRenderable)),
        ?assert(is_integer(NativeVisualId)),
        ?assert(is_integer(NativeVisualType)),
        ?assert(is_integer(RedSize)),
        assert_is_bitmask(
            RenderableType,
            [opengl_bit, opengl_es_bit, opengl_es2_bit, openvg_bit]
        ),
        ?assert(is_integer(SampleBuffers)),
        ?assert(is_integer(Samples)),
        ?assert(is_integer(StencilSize)),
        assert_is_bitmask(
            SurfaceType,
            [
                multisample_resolve_box_bit,
                pbuffer_bit,
                pixmat_bit,
                swap_behavior_preserved_bit,
                vg_alpha_format_pre_bit,
                vg_colorspace_linear_bit,
                window_bit
            ]
        ),
        assert_is_enum(TransparentType, [none, transparent_rgb]),
        ?assert(is_integer(TransparentRedValue)),
        ?assert(is_integer(TransparentGreenValue)),
        ?assert(is_integer(TransparentBlueValue)),

        io:format(user, "AlphaSize: ~p~n", [AlphaSize]),
        io:format(user, "AlphaMaskSize: ~p~n", [AlphaMaskSize]),
        io:format(user, "BindToTextureRgb: ~p~n", [BindToTextureRgb]),
        io:format(user, "BindToTextureRgba: ~p~n", [BindToTextureRgba]),
        io:format(user, "BlueSize: ~p~n", [BlueSize]),
        io:format(user, "BufferSize: ~p~n", [BufferSize]),
        io:format(user, "ColorBufferType: ~p~n", [ColorBufferType]),
        io:format(user, "ConfigCaveat: ~p~n", [ConfigCaveat]),
        io:format(user, "ConfigId: ~p~n", [ConfigId]),
        io:format(user, "Conformant: ~p~n", [Conformant]),
        io:format(user, "DepthSize: ~p~n", [DepthSize]),
        io:format(user, "GreenSize: ~p~n", [GreenSize]),
        io:format(user, "Level: ~p~n", [Level]),
        io:format(user, "LuminanceSize: ~p~n", [LuminanceSize]),
        io:format(user, "MaxPbufferWidth: ~p~n", [MaxPbufferWidth]),
        io:format(user, "MaxPbufferHeight: ~p~n", [MaxPbufferHeight]),
        io:format(user, "MaxPbufferPixels: ~p~n", [MaxPbufferPixels]),
        io:format(user, "MaxSwapInterval: ~p~n", [MaxSwapInterval]),
        io:format(user, "MinSwapInterval: ~p~n", [MinSwapInterval]),
        io:format(user, "NativeRenderable: ~p~n", [NativeRenderable]),
        io:format(user, "NativeVisualId: ~p~n", [NativeVisualId]),
        io:format(user, "NativeVisualType: ~p~n", [NativeVisualType]),
        io:format(user, "RedSize: ~p~n", [RedSize]),
        io:format(user, "RenderableType: ~p~n", [RenderableType]),
        io:format(user, "SampleBuffers: ~p~n", [SampleBuffers]),
        io:format(user, "Samples: ~p~n", [Samples]),
        io:format(user, "StencilSize: ~p~n", [StencilSize]),
        io:format(user, "SurfaceType: ~p~n", [SurfaceType]),
        io:format(user, "TransparentType: ~p~n", [TransparentType]),
        io:format(user, "TransparentRedValue: ~p~n", [TransparentRedValue]),
        io:format(user, "TransparentGreenValue: ~p~n", [TransparentGreenValue]),
        io:format(user, "TransparentBlueValue: ~p~n", [TransparentBlueValue])
    end, Configs),

    ok.
