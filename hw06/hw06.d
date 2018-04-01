src/cache_handler.erl:: src/cache_server.erl; @touch $@
src/ets_cache.erl:: src/cache_handler.erl; @touch $@

COMPILE_FIRST += cache_server cache_handler
