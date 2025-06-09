-module(egl_get_display_test).
-include_lib("eunit/include/eunit.hrl").

egl_get_display_test() ->
    _Display = egl:get_display(default_display),

    ok.
