-module(utils).
-author("sas").

-export([r200/2, r204/1, r400/1, r404/1, r405/1, send_json_error/1]).

r200(Req, Data) ->
    cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(Data), Req).

r204(Req) ->
    cowboy_req:reply(204, Req).

r400(Req) ->
    cowboy_req:reply(400, Req).

r404(Req) ->
    cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(<<"Not found">>), Req).

r405(Req) ->
    cowboy_req:reply(405, Req).

send_json_error(Req) ->
    cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>},
        jiffy:encode(<<"Bad json">>), Req).
