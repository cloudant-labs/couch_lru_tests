-module(couch_lru_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-behaviour(proper_statem).


-export([
    couch_lru_works/0
]).

-export([
    command/1,
    initial_state/0,
    next_state/3,
    postcondition/3,
    precondition/2
]).


-define(TEST_MOD, couch_lru_dict).


couch_lru_works_test_() ->
    {
        timeout,
        100000,
        ?_assertEqual(
            true,
            proper:quickcheck(
                ?MODULE:couch_lru_works(),
                [{to_file, user}, {numtests, 100}]))
    }.


couch_lru_works() ->
    ?FORALL(Cmds, commands(?MODULE),
        ?TRAPEXIT(
            begin
                Lru = ?TEST_MOD:new(),
                {History, State, Result} =
                        run_commands(?MODULE, Cmds, [{lru, Lru}]),
                ?WHENFAIL(io:format(standard_error,
                        "~n~nHistory: ~w\nState: ~w\nResult: ~w~n~n",
                        [History,State,Result]),
                    aggregate(command_names(Cmds), Result =:= ok))

            end)).


initial_state() ->
    [].


command(_) ->
    frequency([
        {33, {call, ?TEST_MOD, push, [binary(), {var, lru}]}},
        {33, {call, ?TEST_MOD, pop, [{var, lru}]}},
        {33, {call, ?TEST_MOD, update, [binary(), {var, lru}]}},
        {1, {call, ?TEST_MOD, to_list, [{var, lru}]}}
    ]).


precondition(_, _) ->
    true.


next_state(Lru0, _RV, {call, _, push, [B, _]}) ->
    Lru1 = lists:delete(B, Lru0),
    [B | Lru1];
next_state(Lru, _RV, {call, _, pop, [_]}) ->
    Lru;
next_state(Lru0, _RV, {call, _, pop, [_]}) ->
    [_ | Lru1] = lists:reverse(Lru0),
    lists:reverse(Lru1);
next_state(Lru0, _RV, {call, _, update, [B, _]}) ->
    Lru1 = lists:delete(B, Lru0),
    [B | Lru1];
next_state(Lru, _RV, {call, _, to_list, [_]}) ->
    Lru.


postcondition(Lru, {call, _, push, _}, Result) ->
    ?TEST_MOD:to_list(Result) == Lru;
postcondition([], {call, _, pop, _}, {undefined, T}) ->
    [] == ?TEST_MOD:to_list(T);
postcondition(Lru, {call, _, pop, _}, Result) ->
    ?TEST_MOD:to_list(Result) == Lru;
postcondition(Lru, {call, _, update, _}, Result) ->
    ?TEST_MOD:to_list(Result) == Lru;
postcondition(Lru, {call, _, to_list, _}, Result) ->
    Result == Lru.
