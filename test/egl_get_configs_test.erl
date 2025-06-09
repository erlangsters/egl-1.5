-module(egl_get_configs_test).
-include_lib("eunit/include/eunit.hrl").

egl_get_configs_test() ->
    Display = egl:get_display(default_display),
    {ok, _Version} = egl:initialize(Display),

    {ok, Configs} = egl:get_configs(Display),
    test = Configs,

    ok.
