-module(loggrep_conf).
-behaviour(gen_server).

-export([logs/0,
         log_files/1
        ]).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(CONF_NAME, "./priv/loggrep.conf").

-record(state, {logs = []}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec logs() -> [any()].
logs() ->
    gen_server:call(?MODULE, logs).

-spec log_files(file:filename()) -> [file:filename()] | undefined.
log_files(Name) ->
    gen_server:call(?MODULE, {log_files, Name}).

init([]) ->
    Conf = read_config(?CONF_NAME),
    Logs = proplists:get_value(logs, Conf, []),
    LogsPrepared = [{Name, prepare_log(File)} || {Name, File} <- Logs],
    {ok, #state{ logs = LogsPrepared}}.

handle_call(logs, _From, #state{logs = Logs} = State) ->
    Reply = proplists:get_keys(Logs),
    {reply, Reply, State};

handle_call({log_files, Name}, _From, #state{logs = Logs} = State) ->
    FS = case proplists:get_value(Name, Logs) of
             undefined ->
                 undefined;
             {files, Files} ->
                 [F || F <- Files, filelib:is_regular(F)];
             File ->
                 case filelib:is_regular(File) of
                     true ->
                         [File];
                     false ->
                         case filelib:is_dir(File) of
                             true ->
                                 {ok, Files} = file:list_dir(File),
                                 [F || F <- Files, filelib:is_regular(filename:join(File, F))];
                             false ->
                                 case filelib:wildcard(File) of
                                     [] ->
                                         undefined;
                                     List ->
                                         [F || F <- List, filelib:is_regular(filename:join(File, F))]
                                 end
                         end
                 end
         end,                            
    {reply, FS, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

prepare_log(File) ->
    File.

read_config(Filename) ->
    case file:consult(Filename) of
        {ok, Data} ->
            Data;
        {error, Error} ->
            io:format("Config read error:~p~n", [Error]),
            error({parse_error, Error})
    end.
