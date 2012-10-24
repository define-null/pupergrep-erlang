-module(loggrep).
-behaviour(application).

-export([start/0,
         start/2,
         stop/1
        ]).

-export([service_logs/3,
         service_subscribtion/3,
         service_subscribtion_send/3
        ]).

start() ->
    application:start(loggrep).

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
                   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}],
    
    VhostRoutes = [{[<<"multiplex">>, '...'], sockjs_cowboy_handler, SockjsState},
                   {[], cowboy_http_static, [{file, "index.html"} | StaticState]},
                   {['...'], cowboy_http_static, StaticState}],
    
    Routes = [{'_',  VhostRoutes}], % any vhost

    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport, [{port,     Port}],
                          cowboy_http_protocol, [{dispatch, Routes}]),

    io:format(" [*] Running at http://localhost:~p~n", [Port]),
    
    loggrep_sup:start_link().
    
stop(_State) ->
    ok.

%%------------------------------------------------------------------------------
%%
%%------------------------------------------------------------------------------

%% {"name":"logs","args":
%%       [{"logs":[{"name":"access_log (static files for pupergrep)"},
%%                 {"name":"html pewpew","type":"html"},
%%                 {"name":"unfollowr access_log"},
%%                 {"name":"uptime (fast)"}]}]
%% }
service_logs(Conn, init, State) ->
    Logs = loggrep_conf:logs(), 
    Data = [{logs, [[{name, X}] || X <- Logs] }],
    Conn:send(jsx:term_to_json(Data)),
    {ok, State};
service_logs(_Conn, {recv, _Data}, State) ->
    {ok, State};
service_logs(_Conn, closed, State) ->
    {ok, State}.

service_subscribtion(Conn, init, State) ->
    {ok, Client} = loggrep_clients_sup:start_client(Conn),
    {ok, [{client, Client} | State]};
service_subscribtion(Conn, {recv, Data}, State) ->
    case parse_request(Data) of
        {ok, Request} ->
            command(Conn, Request, State);
        {error, _Error} ->
            {ok, State}
    end;
service_subscribtion(_Conn, closed, State) ->
    loggrep_clients_sup:stop_client(proplists:get_value(client, State)),
    {ok, State}.

%%------------------------------------------------------------------------------
%%
%%-----------------------------------------------------------------------------

%% @doc Parse request.
parse_request(Data) ->
    Js = list_to_binary(Data),
    case jsx:is_json(Js) of
        true ->
            {ok, jsx:decode(Js)};
        false ->
            {error, not_json}
    end.

command(Conn, [{<<"name">>, Filename}], State) ->
    subscribe_log(Conn, Filename, State).

subscribe_log(_Conn, Filename, State) ->
    Client = proplists:get_value(client, State),
    %% Можно сделать логику подписывания только на один лог            
    loggrep_client:subscribe(Client, Filename, fun service_subscribtion_send/3),
    Filenames = proplists:get_value(files, State),
    State1 = [{files, [Filename | Filenames]} | proplists:delete(files, State)],
    {ok, State1}.

service_subscribtion_send(Conn, _Filename, Msg) ->
    Conn:send("{\"line\" : \"" ++ Msg ++ "\"}").

