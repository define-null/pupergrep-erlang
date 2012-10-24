%% File    : loggrep_tailf.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : 

-module(loggrep_tailf).
-behaviour(gen_server).

%% API
-export([start_link/0,
         subscribe/2,
         unsubscribe/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-record(state, {}).

%%------------------------------------------------------------------------------
%% Api
%%------------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec subscribe(any(), pid()) -> {ok, pid()} | {error, any()}.
subscribe(LogName, SubPid) ->
    case loggrep_tailf_worker:sub(LogName, SubPid) of
        {error, no_name} ->
            case gen_server:call(?MODULE, {get_instance, LogName}) of
                {ok, LogInst} ->
                    loggrep_tailf_worker:sub(LogName, LogInst, SubPid);
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, LogPid} ->
            {ok, LogPid}
    end.

-spec unsubscribe(any(), pid()) -> ok.
unsubscribe(LogName, SubPid) ->
    loggrep_tailf_worker:unsub(LogName, SubPid),
    ok.

%%------------------------------------------------------------------------------
%% Genserver callbacks
%%------------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({get_instance, LogName}, _From, State) ->
    Resp = case gproc:lookup_local_name(loggrep_tailf_worker:gproc_name(LogName)) of
               undefined ->
                   Files = loggrep_conf:log_files(LogName),
                   loggrep_tailf_sup:start_tailf_worker(LogName, Files);
               Pid ->
                   {ok, Pid}
           end,
    {reply, Resp, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
