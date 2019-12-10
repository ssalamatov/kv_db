-module(db_worker).
-behavior(gen_server).

-export([
    start_link/1,
    init/1,
    handle_cast/2,
    handle_call/3,
    handle_info/2,
    terminate/2]).


-record(state, {connection,
                collection}).

start_link(Db) ->
    gen_server:start({global, Db}, ?MODULE, [], []).

init([]) ->
    {ok, Config} = application:get_env(db, mongodb),
    {ok, Connection} = mc_worker_api:connect(Config),
    Collection = <<"db">>,
    {ok, #state{connection=Connection,
        collection=Collection}}.

%% read all keys
handle_call(aread, _From, #state{collection=C, connection=Conn}=State) ->
    ValFound = case mc_worker_api:find(Conn, C, #{},
        #{projector => #{<<"_id">> => false}}) of
                   {ok, Val} -> merge_maps(mc_cursor:rest(Val));
                   Other -> Other
               end,
    {reply, ValFound, State};

%% find one key in db
handle_call({read, Key}, _From, #state{collection=C, connection=Conn}=State) ->
    ValFound = case mc_worker_api:find(Conn, C, #{<<"key">> => Key},
        #{projector => #{<<"key">> => false, <<"_id">> => false}}) of
                   {ok, Val} -> mc_cursor:rest(Val);
                   Other -> Other
               end,
    {reply, ValFound, State};

%% find multiple keys in db
handle_call({mread, Keys}, _From, #state{collection=C, connection=Conn}=State) ->
    case mc_worker_api:find(Conn, C, #{},
        #{projector => #{<<"_id">> => false}}) of
                   {ok, Val} ->
                       ValFound = mc_cursor:rest(Val),
                       {reply, lists:foldl(fun(M, Acc) ->
                           case lists:member(maps:get(<<"key">>, M), Keys) of
                               true -> [maps:get(<<"value">>, M) | Acc];
                               _ -> Acc
                           end end, [], ValFound), State};
                    _ -> {reply, [], State}
               end;

%% write data
handle_call({write, M}, _From, #state{collection=C, connection=Conn}=State) ->
    R = case mc_worker_api:insert(Conn, C, create_list(M)) of
              {{true, _}, Val} ->
                  merge_maps(Val);
              _ -> error
          end,
    {reply, R, State};

handle_call({delete, Key}, _From, #state{collection=C, connection=Conn}=State) ->
    {true, R} = mc_worker_api:delete(Conn, C, #{<<"key">> => Key}),
    {reply, rows_affected(R), State};

%% remove all collection
handle_call(clean, _From, #state{collection=C, connection=Conn}=State) ->
    {true, _} = mc_worker_api:delete(Conn, C, #{}),
    {reply, ok, State};

handle_call(_, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection=Conn}) ->
    mongo_api:disconnect(Conn).

rows_affected(R) ->
    case maps:find(<<"n">>, R) of
        {ok, 0} -> error;
        _ -> ok
    end.

create_list(L) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            [#{<<"key">> => Key, <<"value">> => Value} | Acc]
        end, [], L).

merge_maps(L) ->
    lists:foldl(
        fun(M, Acc) ->
            maps:put(maps:get(<<"key">>, M),
                maps:get(<<"value">>, M), Acc) end, #{}, L).
