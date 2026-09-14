-module(egl_get_display_test).
-include_lib("eunit/include/eunit.hrl").

egl_get_display_test() ->
    Display = egl:get_display(default_display),
    Display = egl:get_display(default_display),
    ?assertError(badarg, egl:get_display(not_a_display)),

    ok.
