-module(egl_surface_attrib_test).
-include_lib("eunit/include/eunit.hrl").

egl_surface_attrib_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    ok.
