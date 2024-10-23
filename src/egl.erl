%%
%% Copyright (c) 2025, Byteplug LLC.
%%
%% This source file is part of a project made by the Erlangsters community and
%% is released under the MIT license. Please refer to the LICENSE.md file that
%% can be found at the root of the project repository.
%%
%% Written by Jonathan De Wachter <jonathan.dewachter@byteplug.io>
%%
-module(egl).
-export([foo/0]).
-nifs([foo/0]).
-on_load(init/0).

init() ->
    % XXX: Generated library should be `egl.so` but erlang.mk won't allow that.
    ok = erlang:load_nif("./priv/egl_1_5", 0).

foo() ->
    erlang:nif_error(nif_library_not_loaded).
