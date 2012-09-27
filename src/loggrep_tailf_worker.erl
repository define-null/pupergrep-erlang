%% File    : loggrep_tailf_worker.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : Worker process which aim
%%

-module(loggrep_tailf_worker).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2,
         sub/1,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port, name, subs = sets:new()}).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).
    
start_link(Name, Files) ->
    gen_server:start_link(?MODULE, [Name, Files], []).

stop(Name) ->
    case gproc:lookup_local_name({?MODULE, Name}) of
        undefined ->
            ok;
        Pid ->
            gen_server:cast(Pid, stop)
    end.

sub(Pid) ->
    gen_server:call(?MODULE, {sub, Pid}).

init([Name]) ->
    Files = loggrep_conf:get_files(Name),
    init([Name, Files]);
init([Name, Files]) ->
    gproc:add_local_name({?MODULE, Name}),
    Port = erlang:open_port({spawn, string:concat("tail -f ", string:join(Files," "))},
                            [{line, 1024}, exit_status, stderr_to_stdout]
                           ),
    {ok, #state{port = Port, name = Name}}.

handle_call({sub, Pid}, _From, #state{subs = Sub} = State) ->
    NewSub = case sets:is_element(Pid, Sub) of
                 true ->
                     Sub;
                 false ->
                     erlang:monitor(process, Pid),
                     sets:add_element(Pid, Sub)
             end,
    
    {reply, ok, State#state{subs = NewSub}};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, #state{port = Port} = State) ->
    true = erlang:port_close(Port),
    {stop, normal, State#state{port = undefined}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonRef, process, Pid, _Info}, #state{subs = Sub, port = Port} = State) ->
    NewSub = sets:del_element(Pid, Sub),
    case sets:size(NewSub) of
        0 ->
            true = erlang:port_close(Port),
            {stop, normal, State#state{port = undefined, subs = sets:new()}};
        _ ->
            {noreply, State#state{subs = NewSub}}            
    end;
handle_info({Port, {data, {_, Data}}}, #state{port = Port, name = Name} = State) ->
    loggrep_tailf:worker_send(Name, Data),
    {noreply, State};
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_exited, Reason}, State};
handle_info({Port, closed}, #state{port = Port} = State) ->
    {stop, port_closed, State};
handle_info({Port,{exit_status,Status}}, #state{port = Port} = State) ->
    {stop, {port_exit, Status}, State#state{port = undefined}};
handle_info(_Info, State) ->
    io:format("Info extra: ~w", [_Info]),
    {noreply, State}.

terminate(_Reason, #state{port = Port}) ->
    case Port of
        undefined -> ok;
        _ ->
            true = erlang:port_close(Port)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

