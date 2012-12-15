%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------

-module(inpevt).

-export([subscribe/2, unsubscribe/2, get_devices/0,get_devices/1, get_devices/2,i/0]).
-export([add_device/1]).

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

add_device(FileName) ->
    gen_server:call(inpevt_server, { add_device, FileName }).

i() ->
    gen_server:call(inpevt_server, { i }).
