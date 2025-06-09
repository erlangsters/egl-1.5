-module(egl_destroy_surface_test).
-include_lib("eunit/include/eunit.hrl").

egl_destroy_surface_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    ConfigAttribs = [{surface_type, [pbuffer_bit]}],
    {ok, Configs} = egl:choose_config(Display, ConfigAttribs),
    Config = hd(Configs),
    test_egl:print_config(Display, Config),

    SurfaceAttribs = [{width, 1}, {height, 1}],
    {ok, Surface} = egl:create_pbuffer_surface(Display, Config, SurfaceAttribs),
    test_egl:print_surface(Display, Surface),

    ok = egl:destroy_surface(Display, Surface),

    ok.
