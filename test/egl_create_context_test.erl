-module(egl_create_context_test).
-include_lib("eunit/include/eunit.hrl").

egl_create_context_test() ->
    Display = egl:get_display(default_display),
    {ok, {_, _}} = egl:initialize(Display),

    ConfigAttribs = [],
    {ok, Configs} = egl:choose_config(Display, ConfigAttribs),
    Config = hd(Configs),
    test_egl:print_config(Display, Config),

    % XXX: no attributes will not work
    % ContextAttribs = [],
    % Test =
    %     egl:create_context(Display, Config, no_context, ContextAttribs),
    % io:format(user, "error ? ~p~n", [egl:get_error()]),

    ContextAttribs = [
        % XXX: should bindgin support context client version
        % {context_client_version, 3}
        {context_major_version, 3}
    ],
    {ok, Context} =
        egl:create_context(Display, Config, no_context, ContextAttribs),
    test_egl:print_context(Display, Context),

    ok = egl:destroy_context(Display, Context),

    ok.
