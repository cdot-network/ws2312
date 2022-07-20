-module(ws2312).
-behavior(gen_server).

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-export([
         blink/2
        ]).

-type led_handle() :: pid() | undefined.

-record(state, {
    handle :: led_handle()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Trap exits to allow for terminate led animation
    erlang:process_flag(trap_exit, true),

    case spi:start_link("spidev0.0", []) of
        {ok, Handle} ->
            State = #state {
                       handle = Handle
                      },
            {ok, State};
        _ ->
            {err}
    end.

blink(Pid, State) ->
    gen_server:cast(Pid, blink, State).

handle_call(Msg, _From, State) ->
    lager:warning("Unhandled call ~p: ~p", [Msg, State]),
    {noreply, State}.

handle_cast(blink, State = #state{handle = undefined}) ->
    lager:info("blinking with undefined"),
    {noreply, State};
handle_cast(blink, State) ->
    spi:transfer(State#state.handle),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:warning("Unhandled cast ~p: ~p", [Msg, State]),
    {noreply, State}.

    
