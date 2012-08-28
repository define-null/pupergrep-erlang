%% File    : loggrep_conf.erl
%% Author  : Defnull <define.null@gmail.com>
%% Created : 24-08-2012 by Defnull
%% Description : 

-module(loggrep_conf).
-export([logs/0,
         log_files/1
        ]).

read_config() ->
    Filename = "./priv/loggrep.conf",
    {ok, Data} = file:consult(Filename),
    Data.

logs() ->
    ["somelog"].

log_files(Name) ->
    ["/tmp/1.log"].
