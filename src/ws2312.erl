-module(ws2312).
-behavior(gen_server).

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-export([
         blink/0
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

    case spi:start_link("spidev0.0", [{speed_hz, 4000000}]) of
        {ok, Handle} ->
            State = #state {
                       handle = Handle
                      },
            {ok, State};
        _ ->
            {err}
    end.

blink() ->
    gen_server:cast(?MODULE, blink).

handle_call(Msg, _From, State = #state{}) ->
    lager:warning("Unhandled call ~p: ~p", [Msg, State]),
    {noreply, State}.

handle_cast(blink, State = #state{handle = undefined}) ->
    lager:info("blinking with undefined handle"),
    {noreply, State};
handle_cast(blink, State) ->
    xfer_pattern(State),
    {noreply, State};
handle_cast(Msg, State) ->
    lager:warning("Unhandled cast ~p: ~p", [Msg, State]),
    {noreply, State}.

    
xfer_pattern(State) ->
    spi:transfer(State#state.handle, 
                 <<
                   2#00000000,
                   2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#11101000, 2#10001000, 2#10001000, 2#10001000,
                   2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000
                 >>),
    spi:transfer(State#state.handle, <<2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8>>).

