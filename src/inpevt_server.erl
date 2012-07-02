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

-record(drv_dev_id, {
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::list(#cap_spec{})
          }).

-record(device, {
          port::port(),
          fname::string(),
          subscribers = [] ::list(pid()),
          id::string(),
          name::string(),
          bus::atom(),
          vendor::integer(),
          product::integer(),
          version::integer(),
          topology::string(),
          capabilities::list(#cap_spec{})
          }).


-record(state, {
          devices = [] ::list(#device{})
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


-spec activate_event_port(port()) -> ok|{error, illegal_arg | io_error | not_open}.

-spec deactivate_event_port(port()) -> ok|{error, illegal_arg | io_error | not_open}.

-spec add_subscriber(pid(), #device {}, list(#device{})) ->
                            { ok, list(#device{}) } |
                            {error, illegal_arg | io_error | not_open }.

-spec delete_subscriber(pid(), #device {}, list(#device{})) ->
                               { ok , list(#device{}) } |
                               { error, not_found | illegal_arg | io_error | not_open }.

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

    DevList = State#state.devices,
    case lists:keytake(Port, #device.port, DevList) of
        {
          value,
          Device,
          TempDevList
        } ->
            {
              reply,
              ok,
              #state {
                devices = [ delete_subscriber(Pid,
                                              Device,
                                              TempDevList)
                            | TempDevList ]
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
                _ ->
                    { noreply, State }
            end;

        { fevent, _, [ delete ], _Directory, FileName } ->
            case FileName of
               ?INPEVT_PREFIX ++ _ ->
                    { noreply, delete_existing_device(FileName, State) };

                _ ->
                    {noreply, State}
            end;
        _ ->
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

convert_return_value(Bits) ->
    if Bits =:= ?IEDRV_RES_OK -> ok;
       Bits =:= ?IEDRV_RES_ILLEGAL_ARG -> illegal_arg;
       Bits =:= ?IEDRV_RES_IO_ERROR -> io_error;
       Bits =:= ?IEDRV_RES_NOT_OPEN -> not_open;
       true -> unknown_error
    end.


probe_event_file(Directory, FileName, DeviceList) ->
    Path = Directory ++ "/" ++ FileName,

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

        _ ->
            DeviceList
    end.


scan_event_directory(Directory) ->
    fnotify:watch(Directory),  %% Start subscribing to changes.
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
            {ok, [ #device { %% CHECK - Can this be done more elegantly?
                      port=Device#device.port,
                      fname=Device#device.fname,
                      id=Device#device.id,
                      name=Device#device.name,
                      bus=Device#device.bus,
                      vendor=Device#device.vendor,
                      product=Device#device.product,
                      version=Device#device.version,
                      topology=Device#device.topology,
                      capabilities=Device#device.capabilities,
                      subscribers = [ Pid ] } | DevList ] };

        {error, ErrCode } ->
            {error, ErrCode}
    end;

add_subscriber(Pid, Device, DevList) when Device#device.subscribers =/= [] ->
    {ok, [ #device { %% CHECK - Can this be done more elegantly?
              port=Device#device.port,
              fname=Device#device.fname,
              id=Device#device.id,
              name=Device#device.name,
              bus=Device#device.bus,
              vendor=Device#device.vendor,
              product=Device#device.product,
              version=Device#device.version,
              topology=Device#device.topology,
              capabilities=Device#device.capabilities,
              subscribers = [ Pid | Device#device.subscribers] } | DevList ] }.



delete_subscriber(Pid, Device, DeviceList) ->

    %% Is this the last subscriber to be removed
    case Device#device.subscribers of
        [_] ->
            %% Try to delete the subscriber.
            case lists:delete(Pid, Device#device.subscribers) of
                %% Subscriber not found.
                false ->
                    { error, not_found };

                %% Subscriber deleted
                [] ->
                    %% Deactiveate the event port so that we don't get handle_info
                    %% called for this device.
                    case deactivate_event_port(Device#device.port) of
                        ok->
                            { ok,
                              [ #device { %% CHECK - Can this be done more elegantly?
                                   port=Device#device.port,
                                   fname=Device#device.fname,
                                   id=Device#device.id,
                                   name=Device#device.name,
                                   bus=Device#device.bus,
                                   vendor=Device#device.vendor,
                                   product=Device#device.product,
                                   version=Device#device.version,
                                   topology=Device#device.topology,
                                   capabilities=Device#device.capabilities,
                                   subscribers = [] } | DeviceList ] };

                        {error, ErrCode } ->
                            {error, ErrCode}
                    end
            end;

        %% We have more than one subscriber.
        _ ->
            %% Delete the subscriber.
            case lists:delete(Pid, Device#device.subscribers) of
                %% Subscriber not found
                false ->
                    { error, not_found };

                %% Subscriber deleted.
                NewSubscribers ->
                    {ok, [ #device { %% CHECK - Can this be done more elegantly?
                              port=Device#device.port,
                              fname=Device#device.fname,
                              id=Device#device.id,
                              name=Device#device.name,
                              bus=Device#device.bus,
                              vendor=Device#device.vendor,
                              product=Device#device.product,
                              version=Device#device.version,
                              topology=Device#device.topology,
                              capabilities=Device#device.capabilities,
                              subscribers = NewSubscribers } | DeviceList ] }
            end
    end.


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
            lists:map(fun(Sub) -> Sub ! Event end, Device#device.subscribers),
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
                              Sub ! #removed_event { port = Device#device.port } end,
                      Device#device.subscribers),
            port_close(Device#device.port),
            #state { devices = NewDeviceList }

    end.







