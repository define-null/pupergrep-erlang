%% File    : loggref_tailf_sup.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : 

-module(loggrep_tailf_sup).
-behaviour(supervisor).

%% API
-export([start_link/0,
         start_tailf_worker/1, start_tailf_worker/2,
         stop_tailf_worker/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%------------------------------------------------------------------------------
%% Api
%%------------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_tailf_worker(Name) ->
    supervisor:start_child(?MODULE, [Name]).

start_tailf_worker(Name, Files) ->
    supervisor:start_child(?MODULE, [Name, Files]).

stop_tailf_worker(Name) ->
    loggrep_tailf_worker:stop(Name).

%%------------------------------------------------------------------------------
%% Supervisor callbacks
%%------------------------------------------------------------------------------

init([]) ->
    SimpleSpec = {tailf, {loggrep_tailf_worker, start_link, []}, permanent, 2000, worker, [loggrep_tailf_worker]},
    SupFlags = {simple_one_for_one, 1000, 3600},
    {ok, {SupFlags, [SimpleSpec]}}.

    


                                   
