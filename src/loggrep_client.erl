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

-record(state, {conn, logs = dict:new()}).

%%------------------------------------------------------------------------------
%% Api
%%------------------------------------------------------------------------------

start_link(Conn) ->
    gen_server:start_link(?MODULE, [Conn], []).

-spec subscribe(pid(), any(), fun()) -> ok | {error, any()}.
subscribe(ClientServer, LogSender, Handler) ->
    gen_server:call(ClientServer, {sub, LogSender, Handler}).

-spec unsubscribe(pid(), any()) -> ok.
unsubscribe(ClientServer, LogSender) ->
    gen_server:call(ClientServer, {unsub, LogSender}).

-spec stop(pid()) -> ok.
stop(ClientServer) ->
    gen_server:cast(ClientServer, stop).

%%------------------------------------------------------------------------------
%% GenserverApi
%%------------------------------------------------------------------------------

init([Conn]) ->
    {ok, #state{conn = Conn, logs = dict:new()}}.

handle_call({sub, LogName, Handler}, _From, State) ->
    {Response, State1} = sub_impl(LogName, Handler, State),
    {reply, Response, State1};
handle_call({unsub, LogName}, _From, State) ->
    {reply, ok, unsub_impl(LogName, State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({gproc_ps_event, {tailf_event, Filename}, Msg}, #state{conn = Conn, logs = Dict} = State) ->
    case dict:is_key(Filename, Dict) of
        true ->
            {Handler, _} = dict:fetch(Filename, Dict),
            Handler(Conn, Filename, Msg);
        false ->
            ok
    end,
    {noreply, State};
handle_info({'DOWN', MonitorRef, process, _Object, _Info}, #state{logs = Dict} = State) ->
    case find_log_mon(MonitorRef, Dict) of
        undefined ->
            {noreply, State};
        {LogName, Handler} ->
            {ok, State1} = sub_impl(LogName, Handler, State#state{logs = dict:erase(LogName, Dict)}),
            {noreply, State1}
    end;
handle_info(_Info, State) ->
    {noreply, State}. 

terminate(_Reason, #state{conn = Conn}) ->
    Conn:close(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------

sub_impl(LogName, Handler, #state{logs = Dict} = State) ->
    case dict:is_key(LogName, Dict) of
        true ->
            {ok, State};
        false ->
            case loggrep_tailf:subscribe(LogName, self()) of
                {ok, Pid} ->
                    Monref = erlang:monitor(process, Pid),
                    {ok, State#state{logs = dict:store(LogName, {Handler, Monref}, Dict)}};
                {error, Reason} ->
                    {{error, Reason}, State}
            end
    end.

unsub_impl(LogName, #state{logs = Dict} = State) ->
    case dict:is_key(LogName, Dict) of
        false ->
            State;
        true ->
            {_,MonRef} = dict:fetch(LogName, Dict),
            true = erlang:demonitor(MonRef),
            ok = loggrep_tailf:unsubscribe(LogName, self()),
            State#state{logs = dict:erase(LogName, Dict)}
    end.

find_log_mon(MonitorRef, Logs) ->
    dict:fold(fun(LogName, {Handler, MonRef}, _) when MonRef =:= MonitorRef ->
                      {LogName, Handler};
                 (_,_,Acc) ->
                      Acc
              end, undefined, Logs).
