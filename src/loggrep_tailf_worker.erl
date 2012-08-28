%% File    : loggrep_tailf_worker.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : 

-module(loggrep_tailf_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {port, name}).

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
init([Name]) ->
    Files = loggrep_conf:get_files(Name),
    init([Name, Files]);
init([Name, Files]) ->
    gproc:add_local_name({?MODULE, Name}),
    Port = erlang:open_port({spawn, string:concat("tail -f ", string:join(Files," "))},
                            [{line, 1024}, exit_status, stderr_to_stdout]
                           ),
    {ok, #state{port = Port, name = Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(stop, #state{port = Port, name = Name} = State) ->
    true = erlang:port_close(Port),
    loggrep_tailf:worker_exit(Name, stop),
    {stop, normal, State#state{port = undefined}};
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

handle_info({Port, {data, {_, Data}}}, #state{port = Port, name = Name} = State) ->
    loggrep_tailf:worker_send(Name, Data),
    {noreply, State};
handle_info({'EXIT', Port, Reason}, #state{port = Port, name = Name} = State) ->
    loggrep_tailf:worker_exit(Name, {port_exited, Reason}),
    {stop, {port_exited, Reason}, State};
handle_info({Port, closed}, #state{port = Port, name = Name} = State) ->
    loggrep_tailf:worker_exit(Name, port_closed),
    {stop, port_closed, State};
handle_info({Port,{exit_status,Status}}, #state{port = Port, name = Name} = State) ->
    loggrep_tailf:worker_exit(Name, {port_exit, Status}),
    {stop, {port_exit, Status}, State#state{port = undefined}};
handle_info(_Info, State) ->
    io:format("Info extra: ~w", [_Info]),
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
terminate(_Reason, #state{port = Port, name = Name}) ->
    case Port of
        undefined -> ok;
        _ ->
            true = erlang:port_close(Port)
    end,
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
