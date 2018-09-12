#!/usr/bin/env escript
%%! -pa ebin/ -pa deps/khash/ebin

-mode(compile).

-export([
    benchmark_insert/3,
    benchmark_update/4,
    benchmark_drain/3
]).


-define(MODULES, [
    couch_lru_dict,
    couch_lru_ets,
    couch_lru_khash
    %couch_lru_list
]).

-define(SIZES, [
    1,
    10,
    100,
    1000,
    10000,
    50000,
    100000,
    500000
]).

-define(OP_COUNT, 100000).


main(_) ->
    seed(),
    TestMods = shuffle(?MODULES),
    Sizes = shuffle(?SIZES),
    lists:foreach(fun(Mod) ->
        Results = lists:map(fun(Size) ->
            {_, Ref} = spawn_monitor(fun() -> bench(Mod, Size) end),
            receive {'DOWN', Ref, _, _, Result} -> Result end
        end, Sizes),
        display(Mod, Results)
    end, TestMods).


bench(Mod, Size) ->
    Lru0 = Mod:new(),
    {ITime, Lru1} = timer:tc(?MODULE, benchmark_insert, [Mod, Lru0, Size]),
    {UTime, Lru2} = timer:tc(?MODULE, benchmark_update, [Mod, Lru1, Size, ?OP_COUNT]),
    {DTime, _Lru3} = timer:tc(?MODULE, benchmark_drain, [Mod, Lru2, Size]),
    exit({Size, ITime, UTime, DTime}).


benchmark_insert(_Mod, Lru, 0) ->
    Lru;
benchmark_insert(Mod, Lru, Size) ->
    Name = list_to_binary(integer_to_list(Size)),
    NewLru = Mod:push(Name, Lru),
    benchmark_insert(Mod, NewLru, Size - 1).


benchmark_update(_Mod, Lru, _Size, 0) ->
    Lru;
benchmark_update(Mod, Lru, Size, Count) ->
    Name = list_to_binary(integer_to_list(random:uniform(Size))),
    NewLru = Mod:update(Name, Lru),
    benchmark_update(Mod, NewLru, Size, Count - 1).


benchmark_drain(_Mod, Lru, 0) ->
    Lru;
benchmark_drain(Mod, Lru, Size) ->
    {_, NewLru} = Mod:pop(Lru),
    benchmark_drain(Mod, NewLru, Size - 1).


seed() ->
    <<
        I1:32/unsigned-integer,
        I2:32/unsigned-integer,
        I3:32/unsigned-integer
    >> = crypto:strong_rand_bytes(12),
    random:seed({I1, I2, I3}).


display(Mod, Results0) ->
    Results = lists:sort(Results0),
    io:format("~s:~n", [Mod]),
    io:format(" ~8s ~12s ~12s ~12s~n", [size, insert, update, drain]),
    lists:foreach(fun({Size, ITime, UTime, DTime}) ->
        io:format(" ~8b ~12b ~12b ~12b~n", [Size, ITime, UTime, DTime])
    end, Results),
    io:format("~n", []).


shuffle(List) ->
    Tagged = [{random:uniform(), Item} || Item <- List],
    [Item || {_, Item} <- lists:sort(Tagged)].
