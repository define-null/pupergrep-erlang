%% File    : loggrep_clients_sup.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 19-10-2012 by Defnull
%% Description : 

-module(loggrep_clients_sup).
-export([]).

%% API
-export([start_link/0,
         start_client/1,
         stop_client/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% Api
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_client(Conn) ->
    supervisor:start_child(?MODULE, [Conn]).

stop_client(ClientPid) when is_pid(ClientPid) ->
    loggrep_client:stop(ClientPid).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    SimpleSpec = {client, {loggrep_client, start_link, []}, permanent, 2000, worker, [loggrep_client]},
    SupFlags = {simple_one_for_one, 1000, 3600},
    {ok, {SupFlags, [SimpleSpec]}}.

    
