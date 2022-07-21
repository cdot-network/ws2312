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
         color_to_bin/1,
         build_led_color/2
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
 blink_color :: <<_:24>>,
 led_num :: integer(),
 blink_tref :: timer:tref() | undefined
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
                       blink_interval = 1000,
                       blink_color = <<16#000022:24>>,
                       led_num = 5,
                       blink_tref = undefined
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
    {_, NewState} = cancel_blink(State),
    {ok, T} = timer:send_interval(NewState#state.blink_interval, led_blink),
    {noreply, NewState#state{blink_tref = T}}.

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

cancel_blink(State = #state{blink_tref = undefined}) ->
    {ok, State};
cancel_blink(State = #state{}) ->
    case timer:cancel(State#state.blink_tref) of
        {ok, cancel} -> {ok, State#state { blink_tref = undefined }};
        {error, Reason } ->
            lager:warning("failed to cancel blink timer: ~p~n", [Reason]),
            {error, State#state{ blink_tref = undefined }}
    end.


color_to_bin(Color) ->
    color_to_bin(Color, <<>>).

color_to_bin(<<>>, Binary) -> Binary;
color_to_bin(<<1:1, Rest/bits>>, Binary) ->
    %% io:format("1:1 Rest: ~p : Binary: ~p~n", [Rest, Binary]),
    color_to_bin(Rest, <<Binary/bits, 2#1110:4>>); %%  1110 for 1-bit
color_to_bin(<<0:1, Rest/bits>>, Binary) ->
    %% io:format("0:1 Rest: ~p : Binary: ~p~n", [Rest, Binary]),
    color_to_bin(Rest, <<Binary/bits, 2#1000:4>>). %% 1000 for 0-bit

build_led_color(Color, N) ->
    ColorBinary = color_to_bin(Color),
    build_led_binary(ColorBinary, <<0:8>>, N).

build_led_binary(_ColorBinary, Binary, 0) -> <<Binary/bits, 0:48>>;
build_led_binary(ColorBinary, Binary, N) ->
    build_led_binary(ColorBinary, <<Binary/bits, ColorBinary/bits>>, N-1).

xfer_pattern(State = #state{flipped = false}) ->
    ColorBinary = build_led_color(State#state.blink_color, State#state.led_num),
    spi:transfer(State#state.handle,ColorBinary);
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
