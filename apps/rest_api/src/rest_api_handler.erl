-module(rest_api_handler).

-behavior(cowboy_handler).

-export([init/2]).
-export([allowed_methods/2]).

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

init(Req, State) ->
    Method = cowboy_req:method(Req),
    HasBody = cowboy_req:has_body(Req),
    Req0 = create_answer(Method, HasBody, Req),
    {ok, Req0, State}.

create_answer(<<"GET">>, _, Req) ->
    case cowboy_req:binding(key, Req) of
        undefined ->
            case cowboy_req:parse_qs(Req) of
                [] ->
                    utils:r200(Req, db_api:aread());
                Qs ->
                    case lists:keyfind(<<"keys">>, 1, Qs) of
                        {_, Keys} ->
                            utils:r200(Req, db_api:mread(
                                binary:split(Keys,<<",">>)));
                        _ -> utils:r400(Req) end
            end;
        Key ->
            utils:r200(Req, db_api:read(Key))
    end;

create_answer(<<"DELETE">>, _, Req) ->
    case cowboy_req:binding(key, Req) of
        undefined -> utils:r400(Req);
        Key ->
            case db_api:delete(Key) of
                error -> utils:r404(Req);
                _ -> utils:r204(Req)
            end
    end;

create_answer(<<"POST">>, true, Req) ->
    {ok, Body, Req0} = cowboy_req:read_body(Req),
    try jiffy:decode(Body, [return_maps]) of Json ->
        Data = db_api:write(Json),
        utils:r200(Req0, Data)
    catch throw:_ ->
        utils:send_json_error(Req0)
    end;

create_answer(_, _, Req) ->
    %% Method not allowed.
    utils:r405(Req).
