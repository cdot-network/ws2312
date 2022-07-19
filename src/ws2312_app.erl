%%%-------------------------------------------------------------------
%% @doc ws2312 public API
%% @end
%%%-------------------------------------------------------------------

-module(ws2312_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ws2312_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
