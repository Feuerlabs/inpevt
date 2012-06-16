%% Copyright (C) 2011, Feuerlabs, Inc. All rights reserved.
%% Redistribution and use in any form, with or without modification, is strictly prohibited.
%%

-module(inpevt).

-export([open/1,version/1,activate/1,deactivate/1,i/0]).

open(Path) ->
    gen_server:call(inpevt_server, {open, Path }).

version(Descriptor) ->
    gen_server:call(inpevt_server, { version, Descriptor}).

activate(Descriptor) ->
    gen_server:call(inpevt_server, { activate, Descriptor}).

deactivate(Descriptor) ->
    gen_server:call(inpevt_server, { decative, Descriptor}).

i() ->
    gen_server:call(inpevt_server, { i }).
