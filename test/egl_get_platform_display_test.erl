-module(egl_get_platform_display_test).
-include_lib("eunit/include/eunit.hrl").

egl_get_platform_display_test() ->
    Display = egl:get_platform_display(wayland, default_display, []),
    case Display of
        no_display ->
            X11 = egl:get_platform_display(x11, default_display, []),
            case X11 of
                no_display ->
                    ok;
                _ ->
                    {ok, _} = egl:initialize(X11),
                    ok
            end;
        _ ->
            {ok, _} = egl:initialize(Display),
            ok
    end.
