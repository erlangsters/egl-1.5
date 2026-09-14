-module(egl_query_surface_test).
-include_lib("eunit/include/eunit.hrl").

egl_query_surface_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    {ok, [Config | _]} = egl:choose_config(Display, [{surface_type, [pbuffer_bit]}]),
    {ok, Surface} = egl:create_pbuffer_surface(Display, Config, [{width, 1}, {height, 1}]),
    {ok, 1} = egl:query_surface(Display, Surface, width),
    {ok, 1} = egl:query_surface(Display, Surface, height),
    ok = egl:destroy_surface(Display, Surface),

    ok.
