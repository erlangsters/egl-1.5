-module(egl_query_string_test).
-include_lib("eunit/include/eunit.hrl").

egl_query_string_test() ->
    Display = egl:get_display(default_display),
    not_ok = egl:query_string(Display, client_apis),
    not_ok = egl:query_string(Display, vendor),
    not_ok = egl:query_string(Display, version),
    not_ok = egl:query_string(Display, extensions),

    {ok, _Version} = egl:initialize(Display),
    {ok, ClientApis} = egl:query_string(Display, client_apis),
    {ok, Vendor} = egl:query_string(Display, vendor),
    {ok, Version} = egl:query_string(Display, version),
    {ok, Extensions} = egl:query_string(Display, extensions),

    ?assert(is_list(ClientApis)),
    ?assert(is_list(Vendor)),
    ?assert(is_list(Version)),
    ?assert(is_list(Extensions)),

    io:format(user, "ClientApis: ~p~n", [ClientApis]),
    io:format(user, "Vendor: ~p~n", [Vendor]),
    io:format(user, "Version: ~p~n", [Version]),
    io:format(user, "Extensions: ~p~n", [Extensions]),

    ok.

egl_query_string_no_display_test() ->
    not_ok = egl:query_string(no_display, client_apis),
    not_ok = egl:query_string(no_display, vendor),
    {ok, Version} = egl:query_string(no_display, version),
    {ok, Extensions} = egl:query_string(no_display, extensions),

    ?assert(is_list(Version)),
    ?assert(is_list(Extensions)),

    io:format(user, "Version: ~p~n", [Version]),
    io:format(user, "Extensions: ~p~n", [Extensions]),

    ok.
