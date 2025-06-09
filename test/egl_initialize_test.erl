-module(egl_initialize_test).
-include_lib("eunit/include/eunit.hrl").

egl_initialize_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    ok.
