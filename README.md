Caching server on Erlang
=====

An OTP application

Build
-----

    $ rebar3 compile
_______________
Insert data
-----

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key","value":[1,2,3]}' http://localhost:8080/api/cache_server

    {"result": "ok"}

Insert data with a specified TTL:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key2","value":"some text","ttl":420}' http://localhost:8080/api/cache_server

    {"result": "ok"}

Insert data with append copy to Mnesia database on disk:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"insert","key":"some_key_db","value":13,"db":true}' http://localhost:8080/api/cache_server

    {"result": "ok"}


Lookup
-----

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup","key":"some_key"}' http://localhost:8080/api/cache_server

    {"result": [1,2,3]}

Database lookup with raising of data to the ETS:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"db_lookup","key":"some_key_db"}' http://localhost:8080/api/cache_server

    {"result": 13}

Lookup by date:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"lookup_by_date","date_from":"2015/1/1 00:00:00", "date_to":"2025/1/10 23:59:59"}' http://localhost:8080/api/cache_server

    {"result":[{"key":"some_key2","value":"some text"},{"key":"some_key","value":[1,2,3]},{"key":"some_key_db","value":13}]}

Database lookup by date with raising of data to the ETS:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"db_lookup_by_date","date_from":"2015/1/1 00:00:00", "date_to":"2025/1/10 23:59:59"}' http://localhost:8080/api/cache_server

    {"result":{"some_key_db":13}}

Deleting
-----

From database:

    $ curl -H "Content-Type: application/json" -X POST -d '{"action":"db_delete_item","key":"some_key_db"}' http://localhost:8080/api/cache_server

    {"result": "ok"}
