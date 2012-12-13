%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%%-------------------------------------------------------------------
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

-export([add_device/1]).

-define(SERVER, ?MODULE).

-record(syn, { element:: report | config | mt_report | mt_dropped }).


-record(key, { key, code::integer() }).


-record(rel, { element:: x | y | z | rx | ry | rz | hwheel | dial | wheel | misc }).


-record(absinfo, { value::integer(), minimum::integer(), maximum::integer(),
                   fuzz::integer(), flat::integer(), resolution::integer() }).


-record(abs, { element:: x | y | z | rx | ry | rz | throttle | rudder | wheel | gas |
                         brake | hat0x | hat0y | hat1x | hat1y | hat2x | hat2y | hat3x |
                         hat3y | pressure | distance | tilt_x | tilt_y | tool_width | volume | misc |
                         mt_slot | mt_touch_major | mt_touch_minor | mt_width_major | mt_width_minor |
                         mt_orientation | mt_position_x | mt_position_y | mt_tool_type | mt_blob_id |
                         mt_tracking_id | mt_pressure | mt_distance,  spec::#absinfo{} }).


-record(sw, { element:: lid | tablet_mode | headphone_insert | rfkill_all | radio | microphone_insert |
                        dock | lineout_insert | jack_physical_insert | videoout_insert |
                        camera_lens_cover | keypad_slide | front_proximity | rotate_lock |
                        linein_insert }).

-record(cap_spec, { element::#syn {} | #key {} | #rel {} | #abs {} | #sw {} }).

-record(drv_dev_id, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::[#cap_spec{}]
          }).

-record(subscriber, {
          pid::pid(),
          mref::reference()
         }).

-record(device, {
          port::port(),
          fname::string(),
          subscribers::[#subscriber{}],
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::[#cap_spec{}]
          }).


-record(state, {
          devices = [] ::[#device{}]
         }).

%% From /usr/include/linux/input.h
-record(input_event,
        {
          port, sec, usec, type, code_sym, code_num, value
        }).

-record(removed_event,
        {
          port
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
-define (INPEVT_PREFIX, "event").
-define (INPEVT_TOUCHSCREEN, "touchscreen").


-type port_result():: ok|{error, illegal_arg | io_error | not_open}.
-spec activate_event_port(port()) -> port_result().

-spec deactivate_event_port(port()) -> port_result().

-spec add_subscriber(pid(), #device {}, [#device{}]) ->
                            { ok, [#device{}] } |
                            {error, illegal_arg | io_error | not_open }.

-spec delete_subscriber_from_port(pid(), port(), [#device{}]) -> [#device{}].

-spec delete_subscriber_from_device(pid(), #device{} ) -> #device{}.
-spec delete_terminated_subscriber(pid(), [#device{}]) -> [#device{}].

-spec filter_cap_key(#device {}, { string(), [#device{}]}) -> [#device{}].
-spec filter_cap_spec(#device {}, { string(), [#device{}]}) -> [#device{}].


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
    process_flag(trap_exit, true),
    io:fwrite("priv: ~p~n", [code:priv_dir(inpevt)]),
    Res = erl_ddll:load(code:priv_dir(inpevt), ?INPEVT_DRIVER),
    io:fwrite("Res: ~p~n", [Res]),
    case Res of
	 ok -> {ok, #state{}};
	{ _error, Error } -> { stop, Error };
	_ -> { stop, unknown }

    end.


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
                                Device,
                                TempState) of
                { ok, NewDevList } -> {
                  reply,
                  ok,
                  #state {
                    devices = NewDevList
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
    {
      reply,
      ok,
      #state { devices = delete_subscriber_from_port(Pid, Port, State#state.devices) }
    };

handle_call({ get_devices }, _From, State) ->
    get_devices(State);

handle_call({ get_devices, CapKey }, _From, State) ->
    get_devices(State, CapKey);

handle_call({ get_devices, CapKey, CapSpec }, _From, State) ->
    get_devices(State, CapKey, CapSpec);

handle_call({ add_device, FileName }, _From, State) ->
    io:fwrite("FileName: ~p~n", [FileName]),
    add_new_device(filename:dirname(FileName),
		   filename:basename(FileName),
		   State);



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
        #input_event {} ->
            dispatch_event(Info, State),
            {noreply, State};

        %% Since we are talking about simple USB detection here, we can probably
        %% expect the create atom to be the sole list member.
        { fevent, _, [ create ], Directory, FileName } ->
            case FileName of
               ?INPEVT_PREFIX ++ _ ->
                    { noreply, add_new_device(Directory, FileName, State) };

               ?INPEVT_TOUCHSCREEN ++ _ ->
                    { noreply, add_new_device(Directory, FileName, State) };

                _ ->
                    { noreply, State }
            end;

        { fevent, _, [ delete ], _Directory, FileName } ->
            case FileName of
               ?INPEVT_PREFIX ++ _ ->
                    { noreply, delete_existing_device(FileName, State) };

               ?INPEVT_TOUCHSCREEN ++ _ ->
                    { noreply, delete_existing_device(FileName, State) };

                _ ->
                    {noreply, State}
            end;

        %% Monitor trigger
        { _, _, process, Pid, _ } ->
            {
          noreply,
          #state {
            devices = delete_terminated_subscriber(Pid, State#state.devices)
           }
        };

        _X ->
            io:format("handle_info(): ~p\n", [_X]),
            {noreply, State}
    end.


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

add_device(FileName) ->
    gen_server:call(?SERVER, { add_device, FileName }).

convert_return_value(Bits) ->
    if Bits =:= ?IEDRV_RES_OK -> ok;
       Bits =:= ?IEDRV_RES_ILLEGAL_ARG -> illegal_arg;
       Bits =:= ?IEDRV_RES_IO_ERROR -> io_error;
       Bits =:= ?IEDRV_RES_NOT_OPEN -> not_open;
       true -> unknown_error
    end.


probe_event_file(Directory, FileName, DeviceList) ->
    Path = filename:join(Directory,FileName),

    Port = open_port({spawn, ?INPEVT_DRIVER}, []),

    { Res, ReplyID } = event_port_control(Port, ?IEDRV_CMD_PROBE, [Path]),

    case Res of
        ok ->
            receive
                { device_info, _, ReplyID, DrvDev } ->
                    [ #device {
                         port=Port,
                         fname=FileName,
                         subscribers = [],
                         id=DrvDev#drv_dev_id.id,
                         name=DrvDev#drv_dev_id.name,
                         bus=DrvDev#drv_dev_id.bus,
                         vendor=DrvDev#drv_dev_id.vendor,
                         product=DrvDev#drv_dev_id.product,
                         version=DrvDev#drv_dev_id.version,
                         topology=DrvDev#drv_dev_id.topology,
                         capabilities=DrvDev#drv_dev_id.capabilities
                        } | DeviceList ]
            end;
        Res ->
            DeviceList
    end.

match_and_probe_file(Directory, FileName, DeviceList) ->
    case FileName of
        ?INPEVT_PREFIX ++ _ ->
            probe_event_file(Directory, FileName, DeviceList);

        ?INPEVT_TOUCHSCREEN ++ _ ->
            probe_event_file(Directory, FileName, DeviceList);

        _ ->
            io:fwrite("No match on: ~p\n", [?INPEVT_PREFIX]),
            DeviceList
    end.


scan_event_directory(Directory) ->
    fnotify:watch(Directory),  %% Start subscribing to changes.
    io:fwrite("Scanning: ~p\n", [Directory]),
    case file:list_dir(Directory) of
        { ok, List } ->
            Devices = lists:foldl(fun(FileName, Acc) ->
                                          match_and_probe_file(Directory, FileName, Acc)
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
add_subscriber(Pid, Device, DevList) when Device#device.subscribers =:= [] ->
    case activate_event_port(Device#device.port) of
        ok ->
            add_additional_subscribers(Pid, Device, DevList);

        {error, ErrCode } ->
            {error, ErrCode}
    end;

add_subscriber(Pid, Device, DevList) when Device#device.subscribers =/= [] ->
    add_additional_subscribers(Pid, Device, DevList).

add_additional_subscribers(Pid, Device, DevList) ->
    MonitorRef = erlang:monitor(process, Pid),
    {
      ok,
      [ Device#device {
          subscribers = [ #subscriber{ pid = Pid, mref = MonitorRef } |
                          Device#device.subscribers]
         } | DevList ]
    }.

delete_subscriber_from_port(Pid, Port, DeviceList) ->
    case lists:keytake(Port, #device.port, DeviceList) of
        {
          value,
          Device,
          TempDevList
        } ->
            [ delete_subscriber_from_device(Pid, Device) | TempDevList ];

        _ ->
            DeviceList
    end.


delete_subscriber_from_device(Pid, Device) ->
    %% Is this the last subscriber to be removed
    case Device#device.subscribers of
        [Subscriber] ->
            %% Deactiveate the event port so that we don't get handle_info
            %% called for this device.
            demonitor(Subscriber#subscriber.mref),
            deactivate_event_port(Device#device.port),
            Device#device { subscribers = [] };

        %% We have more than one subscriber.
        _ ->
            %% Delete the subscriber.
            case lists:keytake(Pid, #subscriber.pid, Device#device.subscribers) of
                %% Subscriber not found
                false ->
                    Device;

                %% Subscriber deleted.
                {
                  value,
                  Subscriber,
                  Remainder
                } ->
                    demonitor(Subscriber#subscriber.mref),
                    Device#device { subscribers = Remainder }
            end
    end.

%% Delete a subscriber from all ports in State
delete_terminated_subscriber(Pid, DeviceList) ->
    [ delete_subscriber_from_device(Pid, Device) || Device <- DeviceList ].




activate_event_port(Port) ->
    { Res, _ReplyID } = event_port_control(Port, ?IEDRV_CMD_OPEN, []),
    case Res of
        ok -> ok;
        _ -> { error, Res }
    end.



deactivate_event_port(Port) ->

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
    {Result, Devices } = scan_event_directory(?INPEVT_DIRECTORY),
    case Result of
	ok ->
	    { reply, { ok, Devices }, #state { devices = Devices } };

	_ ->
	    { reply, Result, State }
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
    case proplists:is_defined(CapKey, Device#device.capabilities) of
        true -> { CapKey, [ Device | MatchingDevList ] };
        false -> { CapKey, MatchingDevList }
    end.


filter_cap_spec(Device, { CapKey, CapSpec, MatchingDevList }) ->
    case proplists:get_value(CapKey, Device#device.capabilities) of
        undefined -> { CapKey, CapSpec, MatchingDevList };
        Match->
            case proplists:is_defined(CapSpec, Match) of
                true ->
                    { CapKey, CapSpec, [ Device | MatchingDevList ] };

                false ->
                    { CapKey, CapSpec,  MatchingDevList }
            end

    end.


dispatch_event(Event, State) ->
    case lists:keyfind(Event#input_event.port,
                       #device.port,
                       State#state.devices) of
        false ->
            not_found;
        Device ->
            lists:map(fun(Sub) -> Sub#subscriber.pid ! Event end,
                      Device#device.subscribers),
            found
    end.



add_new_device(Directory, FileName, State) ->
    %% Dies the device already exist? If so just return.
    case lists:keyfind(FileName,
                       #device.fname,
                       State#state.devices) of
        %% Device does not exist, create.
        false ->
             #state {
          devices = probe_event_file(Directory, FileName, State#state.devices)
         };

        %% Device
        _ ->
            State
    end.

delete_existing_device(FileName, State) ->
    %% Locate the device
    case lists:keytake(FileName,
                       #device.fname,
                       State#state.devices) of

        %% Device does not exist, nop.
        false ->
            State;


        %% We found the device. Close its port and remove it from state.
        {value, Device, NewDeviceList } ->
            lists:map(fun(Sub) ->
                              Sub#subscriber.pid ! #removed_event { port = Device#device.port } end,
                      Device#device.subscribers),
            port_close(Device#device.port),
            #state { devices = NewDeviceList }

    end.







