-module(db_api).
-export([read/1, aread/0, mread/1, write/1, clean/0, delete/1]).

read(Key) when is_binary(Key) ->
    gen_server:call({global, db_read}, {read, Key}, infinity).

mread(Keys) when is_list(Keys) ->
    gen_server:call({global, db_read}, {mread, Keys}, infinity).

aread() ->
    gen_server:call({global, db_read}, aread, infinity).

write(M) when is_map(M) ->
    gen_server:call({global, db_write}, {write, M}, infinity).

clean() ->
    gen_server:call({global, db_write}, clean, infinity).

delete(Key) when is_binary(Key) ->
    gen_server:call({global, db_write}, {delete, Key}, infinity).
