%% File    : loggrep_tailf.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : 

-module(loggrep_tailf).
-behaviour(gen_server).

-export([
         subscribe/3,
         unsubscribe/1,
         worker_exit/2,
         worker_send/2
        ]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-record(state, {ets}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

subscribe(Id, Name, Callback) ->
    gen_server:call(?MODULE, {subscribe, Id, Name, Callback}).

unsubscribe(Id) ->
    gen_server:call(?MODULE, {unsubscribe, Id}).

init([]) ->
    Tid = ets:new(loggrep_tailf, [public, set, named_table]),
    {ok, #state{ets = Tid}}.

handle_call({subscribe, Id, Name, Callback}, _From, #state{ets=Tid} = State) ->
    case gproc:lookup_local_name(Name) of
        undefined ->
            Files = loggrep_conf:log_files(Name),
            loggrep_tailf_sup:start_tailf_worker(Name, Files);
        _ ->
            ok
    end,
    true = ets:insert(Tid, {{Id, Name}, Callback}),
    {reply, ok, State};
handle_call({unsubscribe, Id}, _From, #state{ets=Tid} = State) ->
    Names = lists:append(ets:match(Tid, {{Id, '$1'}, '_'})),
    %%[ loggrep_tailf_worker:unpub(Name, Id) || Name <- Names ],
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

worker_exit(Name, Args) ->
    io:format("Worker ~s exit: ~w~n", [Name, Args]).

worker_send(Name, Data) ->
    io:format("Worker ~s send: ~n~s~n", [Name, Data]).    
