-module(egl_surface_attrib_test).
-include_lib("eunit/include/eunit.hrl").

egl_surface_attrib_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    {ok, [Config | _]} = egl:choose_config(Display, [{surface_type, [pbuffer_bit]}]),
    {ok, Surface} = egl:create_pbuffer_surface(Display, Config, [{width, 1}, {height, 1}]),
    case egl:surface_attrib(Display, Surface, swap_behavior, buffer_destroyed) of
        ok ->
            {ok, buffer_destroyed} = egl:query_surface(Display, Surface, swap_behavior);
        not_ok ->
            Error = egl:get_error(),
            ?assert(Error =/= success)
    end,
    ok = egl:destroy_surface(Display, Surface),

    ok.
