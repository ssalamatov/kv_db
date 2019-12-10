%%%-------------------------------------------------------------------
%% @doc reksoft_project public API
%% @end
%%%-------------------------------------------------------------------

-module(rest_api_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
            {'_', [
                {"/db/", rest_api_handler, []},
                {"/db/[:key]", rest_api_handler, []}
            ]}
        ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 9090}],
        #{env => #{dispatch => Dispatch}}
        ),

    rest_api_sup:start_link().

stop(_State) ->
    ok.

%% internal functions