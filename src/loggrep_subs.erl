%% File    : loggrep_subs.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 30-08-2012 by Defnull
%% Description : 

-module(loggrep_subs).
-export([
         sub/2,
         add_client/1,
         delete_client/1
        ]).

add_client(Conn) ->
    Name = make_ref(),
    {ok, Pid} = supervisor:start_child(loggrep_subs_sup, {Name, {loggrep_client, start_link, [Conn]},
                                              permanent, 2000, worker, [loggrep_client]}),
    {Pid, Name}.

sub({Pid, _Name}, Filename) ->
    loggrep_client:sub(Pid, Filename).

delete_client({Pid, Name}) ->
    ok =  ok = supervisor:delete_child(loggrep_subs_sup, Name),
    loggrep_client:stop(Pid).
