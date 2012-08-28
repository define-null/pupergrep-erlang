%% File    : loggrep_sup.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : 

-module(loggrep_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD_LINK(Mod, Type), {Mod, {Mod, start_link, []}, permanent, 2000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = {one_for_one, 1000, 3600},
    {ok, {SupFlags, [
                     ?CHILD_LINK(loggrep_conf, worker),
                     ?CHILD_LINK(loggrep_tailf, worker),
                     ?CHILD_LINK(loggrep_tailf_sup, supervisor)
                    ]}}.

