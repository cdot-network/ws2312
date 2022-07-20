-module(ws2312).

-export([start_link/0]).

-export([init/1]).

-record(state, {
    handle = pid() :: undefined
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init() ->
    %% Trap exits to allow for terminate led animation
    erlang:process_flag(trap_exit, true),

    case spi:start_link("spidev0.0", []) of
        {ok, Handle} ->
            State = #state {
                       handle = Handle
                      },
            State;
        _ ->
            undefined
    end.
