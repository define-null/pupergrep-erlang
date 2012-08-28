%%#!/usr/bin/env escript
%%! -smp disable +A1 +K true -pa ebin deps/sockjs/ebin deps/cowboy/ebin deps/mimetypes/ebin deps/jsx/ebin -input
-module(loggrep).
-mode(compile).

-export([main/1,
         send_message/3
        ]).

main(_) ->
    Port = 8081,
    application:start(sockjs),
    application:start(cowboy),
    application:start(gproc),

    MultiplexState = sockjs_multiplex:init_state(
                       [{"logs",      fun service_logs/3,  []},
                        {"subscribe", fun service_subscribtion/3,  []}
                       ]),

    SockjsState = sockjs_handler:init_state(<<"/multiplex">>, sockjs_multiplex, MultiplexState, []),

    StaticState = [{directory, {priv_dir, loggrep, [<<"www">>]}},
                   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                  ],
    
    VhostRoutes = [
                   {[<<"multiplex">>, '...'], sockjs_cowboy_handler, SockjsState},
                   {[], cowboy_http_static, [{file, "index.html"} | StaticState]},
                   {['...'], cowboy_http_static, StaticState}
                  ],
    
    Routes = [{'_',  VhostRoutes}], % any vhost

    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port,     Port}],
                          cowboy_http_protocol, [{dispatch, Routes}]),
    receive
        _ ->
            ok
    end.

%% --------------------------------------------------------------------------

service_logs(Conn, init, State) ->
    %% {"name":"logs","args":
    %%       [{"logs":[{"name":"access_log (static files for pupergrep)"},
    %%                 {"name":"html pewpew","type":"html"},
    %%                 {"name":"unfollowr access_log"},
    %%                 {"name":"uptime (fast)"}]}]
    %% }
    
    Logs = loggrep_conf:logs(), 
    Data = [{logs,
             [[{name, erlang:list_to_binary(X)}] || X <- Logs]
            }],
    Conn:send(jsx:term_to_json(Data)),
    {ok, State};
service_logs(_Conn, {recv, _Data}, State) ->
    {ok, State};
service_logs(_Conn, closed, State) ->
    {ok, State}.

service_subscribtion(_Conn, init, State) ->
    {ok, State};
service_subscribtion(Conn, {recv, Data}, State) ->
    Js = list_to_binary(Data),
    case jsx:is_json(Js) of
        true ->
            [{<<"name">>, Filename}] = jsx:decode(Js),
            io:format("Filename: ~p~n", [binary_to_list(Filename)]),
            loggrep_tailf:unsubscribe(Conn),
            loggrep_tailf:subscribe(Conn, Filename, fun(Name, Msg) ->
                                                            send_message(Conn, Name, Msg)
                                                    end );
        false ->
            ok
    end,
    {ok, State};
service_subscribtion(Conn, closed, State) ->
    loggrep_tailf:unsubscribe(Conn),
    {ok, State}.

send_message(Conn, _Filename, _Msg) ->
    Conn:send("{\"line\" : \"some string\"}").
