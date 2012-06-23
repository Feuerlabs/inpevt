%%%-------------------------------------------------------------------
%%% @author Magnus Feuer magnus@feuerlabs.com
%%% @copyright (C) 2012, Feuerlabs, Inc
%%% @doc
%%%
%%% @end
%%% Created : 14 Jun 2012 by magnus <magnus@feuerlabs.com>
%%%-------------------------------------------------------------------
-module(inpevt_server).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(syn,
        {
          element::atom(report) | atom(config) | atom(mt_report) | atom(mt_dropped),
          unused_tuple
        }).


-record(key,
        {
          element::atom(), %% Too many keys to list.
          key_code
        }).


-record(rel,
        {
          element::atom(x) | atom(y) | atom(z) | atom(rx)| atom(ry) | atom(rz) |
          atom(hwheel) | atom(dial) | atom(wheel) | atom(misc),
          unused_tuple
        }).


-record(absinfo, {
          value::integer(),
          minimum::integer(),
          maximum::integer(),
          fuzz::integer(),
          flat::integer(),
          resolution::integer()
         }).


-record(abs,
        {
          element::atom(x) | atom(y) | atom(z) | atom(rx) | atom(ry) | atom(rz) |
                   atom(throttle) | atom(rudder) | atom(wheel) | atom(gas) |
                   atom(brake) | atom(hat0x) | atom(hat0y) | atom(hat1x) |
                   atom(hat1y) | atom(hat2x) | atom(hat2y) | atom(hat3x) |
                   atom(hat3y) | atom(pressure) | atom(distance) | atom(tilt_x) |
                   atom(tilt_y) | atom(tool_width) | atom(volume) | atom(misc) |
                   atom(mt_slot) | atom(mt_touch_major) | atom(mt_touch_minor) |
                   atom(mt_width_major) | atom(mt_width_minor) | atom(mt_orientation) |
                   atom(mt_position_x) | atom(mt_position_y) | atom(mt_tool_type) |
                   atom(mt_blob_id) | atom(mt_tracking_id) | atom(mt_pressure) |
                   atom(mt_distance),
          spec::#absinfo{}
        }).


-record(sw, {
          element::atom(lid) | atom(tablet_mode) | atom(headphone_insert) |
                   atom(rfkill_all) | atom(radio) | atom(microphone_insert) |
                   atom(dock) | atom(lineout_insert) | atom(jack_physical_insert) |
                   atom(videoout_insert) | atom(camera_lens_cover) |
                   atom(keypad_slide) | atom(front_proximity) | atom(rotate_lock) |
                   atom(linein_insert),
          unused_tuple
         }).


%% The following capabilities are not yet implemeneted in inpevt_driver.
%% Live with the pain.
-record(led, {
          element::atom(), unused_tuple
         }).

-record(msc, {
          element::atom(), unused_tuple
         }).

-record(snd, {
          element::atom(), unused_tuple
         }).

-record(pwr, {
          element::atom(), unused_tuple
         }).

-record(ff, {
          element::atom(), unused_tuple
         }).

-record(rep, {
          element::atom(), unused_tuple
         }).

-record(ff_status, {
          element::atom(), unused_tuple
         }).

-record(cap_spec, {
          element::#syn {} | #key {} | #rel {} | #abs {} |
                        #msc {} | #sw {} | #led {} | #snd {} |
                        #rep {} | #ff {} | #pwr {} | #ff_status {}
         }).

-record(dev_id, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::list(#cap_spec{})
         } ).

-record(device, {
          port::port(),
          subscribers = [] ::list(pid()),
          dev_id::#dev_id {}
          }).


-record(state, {
          devices = [] ::list(#device{})
         }).

%% From /usr/include/linux/input.h
-record(input_event,
        {
          sec, usec, type, code_sym, code_num, value
        }).

-define (IEDRV_CMD_MASK, 16#0000000F).
-define (IEDRV_CMD_OPEN, 16#00000001).
-define (IEDRV_CMD_CLOSE, 16#00000002).
-define (IEDRV_CMD_PROBE, 16#00000003).

-define (IEDRV_RES_OK, 0).
-define (IEDRV_RES_IO_ERROR, 1).
-define (IEDRV_RES_NOT_OPEN, 2).
-define (IEDRV_RES_ILLEGAL_ARG, 3).

-define (INPEVT_DRIVER, "inpevt_driver").

-define (INPEVT_DIRECTORY, "/dev/input").
-define (INPEVT_PATTERN, "event*").

-spec activate_event_port(port()) -> ok|{error, illegal_arg | io_error | not_open}.

-spec deactivate_event_port(port()) -> ok|{error, illegal_arg | io_error | not_open}.

-spec add_subscriber(pid(), port(),  #dev_id {}, list(pid()), list(#device{})) -> list(#device{}).
-spec delete_subscriber(pid(), port(),  #dev_id {}, list(pid()), list(#device{})) -> list(#device{}).

-spec filter_cap_key(#device {}, { string(), list(#device{})}) -> list(#device{}).
-spec filter_cap_spec(#device {}, { string(), list(#device{})}) -> list(#device{}).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call({open, Device}, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({ subscribe, Port, Pid }, _From, State) ->
    DevList = State#state.devices,
    case lists:keytake(Port, #device.port, DevList) of
        { value, Device,  TempState } ->
            case add_subscriber(Pid,
                                Port,
                                Device#device.dev_id,
                                Device#device.subscribers,
                                TempState) of
                { ok, NewDevList } -> {
                  reply,
                  ok,
                  #state {
                    devices = [ NewDevList ]
                   }
                 };

                { error, ErrCode } -> {
                  reply,
                  { error, ErrCode },
                  State
                 }
            end;
        false -> {
          reply,
          { error, not_found },
          State
         };

        _ ->
            { reply, not_found, State }
    end;

handle_call({ unsubscribe, Port, Pid }, _From, State) ->

    DevList = State#state.devices,
    case lists:keytake(Port, #device.port, DevList) of
        {
          value,
          Device,
          TempState
        } ->
            {
              reply,
              ok,
              #state {
                devices = [ delete_subscriber(Pid,
                                              Port,
                                              Device#device.dev_id,
                                              Device#device.subscribers,
                                              TempState)
                            | TempState ]
               }
            };

        _ ->
            { reply, not_found, State }
    end;



handle_call({ get_devices }, _From, State) ->
    get_devices(State);


handle_call({ get_devices, CapKey }, _From, State) ->
    get_devices(State, CapKey);

handle_call({ get_devices, CapKey, CapSpec }, _From, State) ->
    get_devices(State, CapKey, CapSpec);

handle_call({ init }, _From, State) ->
  handle_call({ init, "/dev/input", "event*"}, _From, State);

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call({open, Device}, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({ close, Port }, _From, State) ->
    { Res, _X } = event_port_control(Port, ?IEDRV_CMD_CLOSE, []),
    { reply, Res, State };


handle_call({ i }, _From, State) ->
    { reply, State, State }.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    case Info of
        #input_event {sec = Sec,
                      usec = Usec,
                      type = Type,
                      code_num = CodeNum,
                      code_sym = CodeSym,
                      value = Value} ->
            EventTS = Sec * 1000000 + Usec,
            io:format("Got data: TS:~w Type:~w CodeNum:~w CodeSym: ~w Value:~w\n",
                      [ EventTS, Type, CodeNum, CodeSym, Value] );
        X ->
            io:format("Unknown event: ~w\n",[ X ])


    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

convert_return_value(Bits) ->
    if Bits =:= ?IEDRV_RES_OK -> ok;
       Bits =:= ?IEDRV_RES_ILLEGAL_ARG -> illegal_arg;
       Bits =:= ?IEDRV_RES_IO_ERROR -> io_error;
       Bits =:= ?IEDRV_RES_NOT_OPEN -> not_open;
       true -> unknown_error
    end.


probe_event_file(File, Acc) ->
    io:format("probe_event_file(): ~s\n", [File]),

    Port = open_port({spawn, ?INPEVT_DRIVER}, []),

    { Res, ReplyID } = event_port_control(Port, ?IEDRV_CMD_PROBE, [File]),

    case Res of
        ok ->
            receive
                { device_info, _, ReplyID, X } ->
                    io:format("Adding device ~w\n", [ X ]),
                    [ #device{ port = Port, subscribers = [], dev_id = X} | Acc ]
            end;
        Res ->
            io:format("Failed to retrieve info for ~s: ~w\n", [ File, Res]),
            Acc
    end.

match_and_probe_file(Directory, File, Acc) ->
    case File of
        "event" ++ _ ->
            probe_event_file(Directory ++ "/" ++ File, Acc);
        _ ->
            io:format("Nope: ~s\n", [ File ] ),
            Acc
    end.


scan_event_directory(Directory) ->
    case file:list_dir(Directory) of
        { ok, List } ->
            Devices = lists:foldl(fun(File, Acc) ->
                                          match_and_probe_file(Directory, File, Acc)
                                  end,
                                  [],
                                  List),
            { ok, Devices };

        { error, Reason } ->
            { error, Reason }
    end.



%%
%% First subscriber added to a device will activate the event port, thus starting the
%% generation of devices.
%%
add_subscriber(Pid, Port, DevID, Subscribers, DevList) when Subscribers =:= [] ->
    case activate_event_port(Port) of
        ok ->
            {ok, [ #device { port=Port, subscribers = [ Pid | Subscribers ], dev_id = DevID }| DevList ] };

        {error, ErrCode } ->
            {error, ErrCode}
    end;

add_subscriber(Pid, Port, DevID, Subscribers, DevList) when Subscribers =/= [] ->
    {ok, [ #device { port=Port, subscribers = [ Pid | Subscribers ], dev_id = DevID }| DevList ] }.


%% Remove the last subscriber.
delete_subscriber(Port, Pid, DevID, [_] = Subscriber, DevList) ->
    case lists:delete(Pid, Subscriber) of
        [] ->
            case deactivate_event_port(Port) of
                ok->
                    { ok,
                      [ #device { port=Port, subscribers = [], dev_id = DevID } |
                        DevList ] };

                {error, ErrCode } ->
                    {error, ErrCode}
            end;

        false ->
                { error, not_found }
        end;

%% Remove subscriber from list greater than one
delete_subscriber(Pid, Port,  DevID, [_|_] = Subscribers, DevList) ->
   case lists:find(Pid, Subscribers) of
       false ->
           { error, not_found };

       NewSubscribers ->
           { ok,
             [ #device { port=Port, subscribers = NewSubscribers, dev_id = DevID } |
               DevList ] }
        end.




activate_event_port(Port) ->
    io:format("activate_event_port(): ~p\n", [Port]),
    { Res, _ReplyID } = event_port_control(Port, ?IEDRV_CMD_OPEN, []),
    case Res of
        ok -> ok;
        _ -> { error, Res }
    end.



deactivate_event_port(Port) ->
    io:format("deactivate_event_port(): ~p\n", [Port]),

    { Res, _ReplyID } = event_port_control(Port, ?IEDRV_CMD_CLOSE, []),
    case Res of
        ok -> ok;
        _ -> { error, Res }
    end.



event_port_control(Port, Command, PortArg) ->
    ResList = port_control(Port, Command, PortArg),
    <<ResNative:32/native>> = list_to_binary(ResList),
    Res = convert_return_value(ResNative bsr 24),
    ReplyID = ResNative band 16#00FFFFFF,
    { Res, ReplyID }.



get_devices(State) when State#state.devices =:= [] ->
    io:format("get_devices([])\n"),
    process_flag(trap_exit, true),
    LoadRes = erl_ddll:load(code:priv_dir(inpevt), ?INPEVT_DRIVER),

    if LoadRes =:= ok;
       LoadRes =:= { error, already_loaded } ->
            {Result, Devices } = scan_event_directory(?INPEVT_DIRECTORY),
            case Result of
                ok ->
                    { reply, { ok, Devices }, #state { devices = Devices } };

                _ ->
                    { reply, Result, State }
            end;
       true -> { reply, LoadRes, State }
    end;


get_devices(State) when State#state.devices =/= [] ->
    {
      reply,
      {ok, State#state.devices},
      State
    }.


get_devices(State, CapKey) ->
    { reply, Result, NewState } = get_devices(State),
    case Result of
        { ok, Devices } ->
            {
              reply,
              {
                ok,
                element(2, lists:foldl(fun filter_cap_key/2, { CapKey, []}, Devices))
              },
              NewState
            };
        Err ->
            { reply, Err, NewState }
    end.


get_devices(State, CapKey, CapSpec) ->
    { reply, Result, NewState } = get_devices(State, CapKey),
    case Result of
        { ok, Devices } ->
            {_, _, Res} = lists:foldl(fun filter_cap_spec/2, { CapKey, CapSpec, []}, Devices),
            {
              reply,
              {
                ok,
                Res
              },
              NewState
            };
        Err ->
            { reply, Err, NewState }
    end.


filter_cap_key(Device, { CapKey, MatchingDevList }) ->
    case proplists:is_defined(CapKey, Device#device.dev_id#dev_id.capabilities) of
        true -> { CapKey, [ Device | MatchingDevList ] };
        false -> { CapKey, MatchingDevList }
    end.


filter_cap_spec(Device, { CapKey, CapSpec, MatchingDevList }) ->
    case proplists:get_value(CapKey, Device#device.dev_id#dev_id.capabilities) of
        undefined -> { CapKey, CapSpec, MatchingDevList };
        Match->
            case proplists:is_defined(CapSpec, Match) of
                true ->
                    { CapKey, CapSpec, [ Device | MatchingDevList ] };

                false ->
                    { CapKey, CapSpec,  MatchingDevList }
            end

    end.

