reksoft_project
=====

A Cowboy REST based application: key-value storage using MongoDB

Build
-----

    $ rebar3 compile

Examples
-----

#### POST 200 /db
```
❯ curl -i -X POST -H 'Content-Type: application/json' "http://localhost:9090/db" -d '{"key1":"value1", "key2":"value2"}'
HTTP/1.1 200 OK
content-length: 33
content-type: application/json
date: Tue, 10 Dec 2019 10:10:14 GMT
server: Cowboy

{"key2":"value2","key1":"value1"}%
```

#### GET 200 /db?keys={key1,key2,key3}
```
❯ curl -i "http://localhost:9090/db?keys=key1,key2,key3"
HTTP/1.1 200 OK
content-length: 19
content-type: application/json
date: Tue, 10 Dec 2019 10:11:09 GMT
server: Cowboy

["value1","value2"]%
```

#### DELETE 204 /db/{key}
```
❯ curl -i -X DELETE "http://localhost:9090/db/key1"
HTTP/1.1 204 No Content
date: Tue, 10 Dec 2019 10:12:20 GMT
server: Cowboy
```

#### GET 200 /db
```
❯ curl -i "http://localhost:9090/db"
HTTP/1.1 200 OK
content-length: 17
content-type: application/json
date: Tue, 10 Dec 2019 10:14:50 GMT
server: Cowboy

{"key2":"value2"}%
```

#### GET 200 /db/{key}
```
❯ curl -i "http://localhost:9090/db/key1"
HTTP/1.1 200 OK
content-length: 22
content-type: application/json
date: Tue, 10 Dec 2019 13:18:25 GMT
server: Cowboy

[{"value":"value111"}]%
```
