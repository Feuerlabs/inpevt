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

-record(state,
        {

        }).

-define (IEDRV_CMD_MASK, 16#0000000F).
-define (IEDRV_CMD_OPEN, 16#00000001).
-define (IEDRV_CMD_CLOSE, 16#00000002).
-define (IEDRV_CMD_ACTIVATE, 16#00000003).
-define (IEDRV_CMD_DEACTIVATE, 16#00000004).
-define (IEDRV_CMD_GET_EVENT_VERSION, 16#00000005).
-define (IEDRV_CMD_GET_BUS_TYPE, 16#00000003).

-define (IEDRV_RES_OK, 0).
-define (IEDRV_RES_IO_ERROR, 1).
-define (IEDRV_RES_NOT_OPEN, 2).
-define (IEDRV_RES_ILLEGAL_ARG, 3).

-define (INPEVT_DRIVER, "inpevt_driver").


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
handle_call({ open, Device }, _From, State) ->

    process_flag(trap_exit, true),
    LoadRes = erl_ddll:load(code:priv_dir(inpevt), ?INPEVT_DRIVER),
    if LoadRes =:= ok; LoadRes =:= { error, already_loaded } ->
            Port = open_port({spawn, ?INPEVT_DRIVER}, []),

            ResList = port_control(Port,
                                   ?IEDRV_CMD_OPEN,
                                   [Device]),

            <<ResNative:32/native>> = list_to_binary(ResList),
            Res = convert_return_value(ResNative bsr 24),
            ReplyID = ResNative band 16#00FFFFFF,
            case Res of
                ok ->
                    receive
                        { device_info, _, ReplyID, X } ->
                            {reply, X, State}
                    end;
                Res -> { reply, Res, State }
            end;
       true -> { reply, LoadRes, State }
    end.




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
handle_info(_Info, State) ->
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
    io:format("convert_return_value(): ~w\n", [Bits]),
    if Bits =:= ?IEDRV_RES_OK -> ok;
       Bits =:= ?IEDRV_RES_ILLEGAL_ARG -> illegal_arg;
       Bits =:= ?IEDRV_RES_IO_ERROR -> io_error;
       Bits =:= ?IEDRV_RES_NOT_OPEN -> not_open;
       true -> unknown_error
    end.
