%% Copyright (C) 2011, Feuerlabs, Inc. All rights reserved.
%% Redistribution and use in any form, with or without modification, is strictly prohibited.
%%

-module(inpevt).

-export([subscribe/2, unsubscribe/2, get_devices/0,get_devices/1, get_devices/2,i/0]).

get_devices() ->
    gen_server:call(inpevt_server, { get_devices }).

get_devices(Capability) ->
    gen_server:call(inpevt_server, { get_devices, Capability}).

get_devices(Capability, CapSpec) ->
    gen_server:call(inpevt_server, { get_devices, Capability, CapSpec}).


subscribe(Port, Pid) ->
    gen_server:call(inpevt_server, { subscribe, Port, Pid }).

unsubscribe(Port,Pid) ->
    gen_server:call(inpevt_server, { unsubscribe, Port, Pid}).

i() ->
    gen_server:call(inpevt_server, { i }).
