-module(egl_get_platform_display_test).
-include_lib("eunit/include/eunit.hrl").

egl_get_platform_display_test() ->
    ?assertError(badarg, egl:get_platform_display(not_a_platform, default_display, [])),
    lists:foreach(fun(Platform) ->
        Display = egl:get_platform_display(Platform, default_display, []),
        ?assert(Display =:= no_display orelse is_reference(Display)),
        case Display of
            no_display ->
                ok;
            _ ->
                case egl:initialize(Display) of
                    {ok, _} ->
                        ok;
                    not_ok ->
                        ok
                end
        end
    end, [wayland, x11, angle]),
    ok.
