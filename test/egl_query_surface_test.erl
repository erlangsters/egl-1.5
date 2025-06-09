-module(egl_query_surface_test).
-include_lib("eunit/include/eunit.hrl").

egl_query_surface_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    ok.
