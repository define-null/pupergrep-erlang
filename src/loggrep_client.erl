%% File    : loggrep_client.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 05-09-2012 by Defnull
%% Description : 

-module(loggrep_client).
-export([]).

-behaviour(gen_server).

%% API
-export([start_link/1,
         subscribe/3,
         unsubscribe/2,
         stop/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {conn,
                logs}).

start_link(Conn) ->
    gen_server:start_link(?MODULE, [[Conn]], []).

subscribe(ClientServer, LogSender, Handler) ->
    gen_server:call(ClientServer, {sub, LogSender, Handler}).

unsubscribe(ClientServer, LogSender) ->
    gen_server:call(ClientServer, {unsub, LogSender}).

stop(ClientServer) ->
    gen_server:cast(ClientServer, stop).

init([Conn]) ->
    process_flag(trap_exit, true),
    {ok, #state{conn = Conn,
                logs = etc:new(logs, [set])
               }}.

handle_call({sub, LogName, Handler}, From, State) ->
    monitor_log(LogName, Handler, State);       

handle_call({unsub, LogName}, From, State) ->
    demonitor_log(LogName, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({LogName, NewChunk}, State) ->
    notify_client_log_new_chunk(LogName, NewChunk, State);

handle_info({'DOWN', MonRef, process, _Pid, Info}, State) ->
    notify_client_log_down(MonRef, Info, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn = Conn}) ->
    Conn:close(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

monitor_log(LogName, Handler, #state{logs = Logs} = State) ->
    Pid = loggrep_tailf:subscribe(self(), LogName),
    MonRef = erlang:monitor(process, Pid),
    {reply, ok, State}.

demonitor_log(LogName, #state{logs = Logs} = State) ->
    MonRef = found_monref(LogName, Logs),
    true = erlang:demonitor(MonRef),
    ok = loggrep_tailf:unsubscribe(self(), LogName),
    %%remove_log(LogName),    
    {reply, ok, State}.   

notify_client_log_new_chunk(LogName, NewChunk, #state{conn = Conn, logs = Logs} = State) ->
    Handler = found_handler(LogName, Logs),
    Handler(Conn, LogName, {new_chunk, NewChunk}),
    State1 = State,
    {noreply, State1}.

notify_client_log_down(MonRef, Info, #state{conn = Conn, logs = Logs} = State) ->
    Handler = found_handler(MonRef, Logs),
    LogName = found_logname(MonRef, Logs),
    Handler(Conn, LogName, log_down),
    State1 = State,
    {noreply, State1}.

found_monref(LogName, Logs) ->
    ok.
found_handler(MonRef, Logs) when is_reference(MonRef) ->
    ok;
found_handler(LogName, Logs) ->
    ok.
found_logname(MonRef, Logs) when is_reference(MonRef) ->
    ok.
