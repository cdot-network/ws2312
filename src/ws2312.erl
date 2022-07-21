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
         blink/1,
         always_on/1,
         off/0,
         color_to_bin/1,
         build_led_color/2
        ]).

-type led_handle() :: pid() | undefined.
-type led_state() :: always_on | blink | flow | off.

-define(BLINK_INTERVAL, 1000). %% msec

-record(state, 
{
 handle :: led_handle(),
 led_state :: led_state(),
 flipped :: boolean(),
 blink_interval :: integer(),
 led_color :: <<_:24>>,
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
                       led_state = off,
                       flipped = false,
                       blink_interval = 1000,
                       led_color = <<16#000022:24>>,
                       led_num = 5,
                       blink_tref = undefined
                      },
            {ok, State};
        _ ->
            {err}
    end.

blink(Color) ->
    gen_server:cast(?MODULE, {blink, Color}).

always_on(Color) ->
    gen_server:cast(?MODULE, {always_on, Color}).

off() ->
    gen_server:cast(?MODULE, off).

handle_call(Msg, _From, State = #state{}) ->
    lager:warning("Unhandled call ~p: ~p", [Msg, State]),
    {noreply, State}.

handle_cast(Msg, State = #state{handle = undefined}) ->
    lager:info("blinking with undefined handle: ~p", [Msg]),
    {noreply, State};
handle_cast({blink, Color}, State) ->
    {_, NewState} = cancel_blink(State),
    NextState = NewState#state{led_state = blink, led_color = Color},
    {ok, T} = timer:send_interval(NextState#state.blink_interval, led_blink),
    xfer_pattern(NextState),
    {noreply, NextState#state{blink_tref = T}};
handle_cast({always_on, Color}, State) ->
    {_, NewState} = cancel_blink(State),
    NextState = NewState#state{led_state = always_on, led_color = Color},
    xfer_pattern(NextState),
    {noreply, NextState};
handle_cast(off, State) ->
    {_, NewState} = cancel_blink(State),
    NextState = NewState#state{led_state = off},
    xfer_off(NextState),
    {noreply, NextState}.

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
    {ok, State#state{ led_state = undefined }};
cancel_blink(State = #state{}) ->
    case timer:cancel(State#state.blink_tref) of
        {ok, cancel} -> {ok, 
                         State#state { blink_tref = undefined,
                                       led_state = undefined }};
        {error, Reason } ->
            lager:warning("failed to cancel blink timer: ~p~n", [Reason]),
            {error, 
             State#state{ blink_tref = undefined, 
                          led_state = undefined }}
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
    build_led_binary(ColorBinary, <<0:16>>, N).

build_led_binary(_ColorBinary, Binary, 0) -> <<Binary/bits, 0:48>>;
build_led_binary(ColorBinary, Binary, N) ->
    build_led_binary(ColorBinary, <<Binary/bits, ColorBinary/bits>>, N-1).

xfer_pattern(State = #state{led_state = always_on}) ->
    ColorBinary = build_led_color(State#state.led_color, State#state.led_num),
    spi:transfer(State#state.handle,ColorBinary);
xfer_pattern(State = #state{led_state = blink, flipped = false}) ->
    ColorBinary = build_led_color(State#state.led_color, State#state.led_num),
    spi:transfer(State#state.handle,ColorBinary);
xfer_pattern(State = #state{led_state = blink, flipped = true}) ->
    xfer_off(State).

xfer_off(State) ->
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
    
