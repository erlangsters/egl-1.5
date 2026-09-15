-module(egl_get_current_surface_test).
-include_lib("eunit/include/eunit.hrl").

egl_get_current_surface_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),
    ok = test_egl:bind_gl_api(),

    {ok, [Config | _]} = egl:choose_config(Display, [{surface_type, [pbuffer_bit]}]),
    {ok, Surface} = egl:create_pbuffer_surface(Display, Config, [{width, 1}, {height, 1}]),
    {ok, Context} = egl:create_context(Display, Config, no_context, [
        {context_major_version, 3}
    ]),
    no_surface = egl:get_current_surface(draw),
    ok = egl:make_current(Display, Surface, Surface, Context),
    Surface = egl:get_current_surface(draw),
    Surface = egl:get_current_surface(read),
    ok = egl:make_current(Display, no_surface, no_surface, no_context),
    no_surface = egl:get_current_surface(draw),
    ok = egl:destroy_context(Display, Context),
    ok = egl:destroy_surface(Display, Surface),

    ok.
