-module(ws2312).
-behavior(gen_server).

-export([start_link/0]).

-export([
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2
        ]).

-export([
         blink/0,
         color_to_bin/1
        ]).

-type led_handle() :: pid() | undefined.
-type led_state() :: always_on | blink | flow.

-define(BLINK_INTERVAL, 1000). %% msec

-record(state, 
{
 handle :: led_handle(),
 led_state :: led_state(),
 flipped :: boolean(),
 blink_interval :: integer(),
 blink_color :: <<_:24>>
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Trap exits to allow for terminate led animation
    erlang:process_flag(trap_exit, true),

    case spi:start_link("spidev0.0", [{speed_hz, 4000000}]) of
        {ok, Handle} ->
            State = #state {
                       handle = Handle,
                       led_state = always_on,
                       flipped = false,
                       blink_interval = 1,
                       blink_color = <<16#FF, 16#00, 16#00>>
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
    timer:send_interval(State#state.blink_interval, led_blink),
    {noreply, State}.

handle_info(led_blink, State) ->
    xfer_pattern(State),
    Flipped = case State#state.flipped of
                  false -> true;
                  _ -> false
              end,
    {noreply, State#state{flipped = Flipped}};
handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p: ~p", [Msg, State]),
    {noreply, State}.

color_to_bin(Color) ->
    color_to_bin(Color, <<>>).

color_to_bin(<<>>, Binary) -> Binary;
color_to_bin(<<1:1, Rest/bits>>, Binary) ->
    color_to_bin(Rest, <<Binary/bits, 2#1110:4>>);
color_to_bin(<<0:1, Rest/bits>>, Binary) ->
    color_to_bin(Rest, <<Binary/bits, 2#1000:4>>).

xfer_pattern(State = #state{flipped = false}) ->
    spi:transfer(State#state.handle, 
                 <<
                   2#00000000,
                   2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#11101000, 2#10001000, 2#10001000, 2#10001000,
                   2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#11101000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000
                 >>),
    spi:transfer(State#state.handle, <<2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8>>);
xfer_pattern(State = #state{flipped = true}) ->
    spi:transfer(State#state.handle, 
                 <<
                   2#00000000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000,
                   2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000, 2#10001000
                 >>),
    spi:transfer(State#state.handle, <<2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8, 2#00000000:8>>).
