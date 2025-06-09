-module(egl_create_pbuffer_surface_test).
-include_lib("eunit/include/eunit.hrl").

egl_create_pbuffer_surface_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    % Pick any config that supports pixel buffer surface.
    ConfigAttribs = [{surface_type, [pbuffer_bit]}],
    {ok, Configs} = egl:choose_config(Display, ConfigAttribs),
    Config = hd(Configs),
    test_egl:print_config(Display, Config),

    % Creating a pixel buffer with no attribute will not work as it needs to be
    % at least 1x1.
    SurfaceAttribs1 = [],
    not_ok = egl:create_pbuffer_surface(Display, Config, SurfaceAttribs1),
    bad_alloc = egl:get_error(),

    SurfaceAttribs = [{width, 1}, {height, 1}],
    {ok, Surface} = egl:create_pbuffer_surface(Display, Config, SurfaceAttribs),
    success = egl:get_error(),
    test_egl:print_surface(Display, Surface),

    ok = egl:destroy_surface(Display, Surface),

    ok.
