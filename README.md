Erlang cache server
=====

An OTP application

Insert data:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key","value":[1,2,3]}' http://localhost:8080/api/cache_server

    {"result": "ok"}

Insert data with a specified TTL:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key2","value":[5,3,2],"ttl":300}' http://localhost:8080/api/cache_server

    {"result": "ok"}

Insert data with append copy to database:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key_db","value":[3,2,1],"db":true}' http://localhost:8080/api/cache_server

    {"result": "ok"}

Lookup:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup","key":"some_key"}' http://localhost:8080/api/cache_server

    {"result": [1,2,3]}

Database lookup with the raising of data in the ETS:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"db_lookup","key":"some_key"}' http://localhost:8080/api/cache_server

    {"result": [1,2,3]}

Lookup by date:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup_by_date","date_from":"2015/1/1 00:00:00", "date_to":"2025/1/10 23:59:59"}' http://localhost:8080/api/cache_server

    {"result":[{"key":"some_key2","value":[5,3,2]},{"key":"some_key","value":[1,2,3]},{"key":"some_key_db","value":[3,2,1]}]}

Build
-----

    $ rebar3 compile
