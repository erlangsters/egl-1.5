-module(egl_choose_config_test).
-include_lib("eunit/include/eunit.hrl").

egl_choose_config_test() ->
    Display = egl:get_display(default_display),
    {ok, _Version} = egl:initialize(Display),

    AttribList = [
        {red_size, 8},
        {green_size, 8},
        {blue_size, 8},
        {alpha_size, 8},
        {depth_size, 24},
        {stencil_size, 8}
    ],
    {ok, Configs} = egl:choose_config(Display, AttribList),
    test = Configs,


    ok.
