-module(loggrep).
-behaviour(application).

-export([start/2,
         stop/1
        ]).
-export([
         service_logs/3,
         service_subscribtion/3,
         service_subscribtion_send/3
        ]).

start(_StartType, _StartArgs) ->
    Port = 8081,
    application:start(sockjs),
    application:start(cowboy),
    application:start(gproc),

    MultiplexState = sockjs_multiplex:init_state( [{"logs",      fun service_logs/3,  []},
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
    loggrep_sup:start_link().
    

stop(_State) ->
    ok.

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

service_subscribtion(Conn, init, State) ->
    Sub = loggrep_subs:add_client(Conn),
    {ok, [{sub, Sub} | State]};
service_subscribtion(Conn, {recv, Data}, State) ->
    case get_log_name(Data) of
        {ok, Filename} ->
            Sub = proplists:get_value(sub, State),
            loggrep_subs:sub(Sub, Filename, fun(Fname, Msg) ->
                                                    service_subscribtion_send(Conn, Fname, Msg)
                                            end),
            {ok, State};
        {error, _Error} ->
            {ok, State}
    end;
service_subscribtion(_Conn, closed, State) ->
    Sub = proplists:get_value(sub, State),
    loggrep_subs:delete_client(Sub),
    {ok, State}.

service_subscribtion_send(Conn, _Filename, _Msg) ->
    Conn:send("{\"line\" : \"some string\"}").

%% --------------------------------------------------------------------------

%% @doc Gets log name from json
get_log_name(Data) ->
    Js = list_to_binary(Data),
    case jsx:is_json(Js) of
        true ->
            [{<<"name">>, Filename}] = jsx:decode(Js),
            {ok, Filename};
        false ->
            {error, not_json}
    end.
        
