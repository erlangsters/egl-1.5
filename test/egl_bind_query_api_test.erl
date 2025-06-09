-module(egl_bind_query_api_test).
-include_lib("eunit/include/eunit.hrl").

egl_bind_query_api_test() ->
    Api = egl:query_api(),
    io:format(user, "EGL API: ~p~n", [Api]),

    ok = egl:bind_api(opengl_api),
    opengl_api = egl:query_api(),

    ok = egl:bind_api(opengl_es_api),
    opengl_es_api = egl:query_api(),

    % OpenVG is not supported by most driver.
    not_ok = egl:bind_api(openvg_api),

    ok.
