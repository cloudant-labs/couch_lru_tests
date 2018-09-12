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


-record(st, {
    dynamic,
    symbolic
}).


-define(TEST_MOD, couch_lru_dict).


couch_lru_works_test_() ->
    {
        timeout,
        100000,
        ?_assertEqual(
            true,
            proper:quickcheck(
                ?MODULE:couch_lru_works(),
                [{to_file, user}, {numtests, 100000}]))
    }.


couch_lru_works() ->
    ?FORALL(Cmds, commands(?MODULE),
        ?TRAPEXIT(
            begin
                Lru = ?TEST_MOD:new(),
                {History, State, Result} =
                        run_commands(?MODULE, Cmds, [{lru, Lru}]),
                ?WHENFAIL(show_error(Cmds, History, State, Result),
                    aggregate(command_names(Cmds), Result =:= ok))

            end)).


show_error(Cmds, History, State, Result) ->
    io:format(standard_error, "~n~n", []),
    io:format(standard_error, "Commands:~n", []),
    lists:foreach(fun(Name) ->
        io:format(standard_error, "    ~w~n", [Name])
    end, Cmds),
    io:format(standard_error, "~n", []),
    io:format(standard_error, "History:~n", []),
    lists:foreach(fun(H) ->
        io:format(standard_error, "    ~w~n", [H])
    end, History),
    io:format(standard_error, "~n", []),
    io:format(standard_error, "State:~n    ~w~n", [State]),
    io:format(standard_error, "~n", []),
    io:format(standard_error, "Result:~n    ~w~n", [Result]),
    io:format(standard_error, "~n~n", []).


initial_state() ->
    #st{
        dynamic = [],
        symbolic = {var, lru}
    }.


command(#st{symbolic = {pop, Var}}) ->
    {call, erlang, element, [2, Var]};
command(#st{dynamic = D, symbolic = S}) ->
    frequency(
        [{33, {call, ?TEST_MOD, update, [oneof(D), S]}} || D /= []] ++
        [
            {33, {call, ?TEST_MOD, push, [binary(), S]}},
            {33, {call, ?TEST_MOD, pop, [S]}},
            {10, {call, ?TEST_MOD, update, [binary(), S]}}
        ]
    ).


precondition(#st{dynamic = Lru}, {call, _, push, [B, _]}) ->
    not lists:member(B, Lru);

precondition(#st{dynamic = []}, {call, _, update, [{call, _, _, _}, _]}) ->
    false;

precondition(_, _) ->
    true.


next_state(#st{dynamic = Lru0} = S, RVal, {call, _, push, [B, _]}) ->
    Lru1 = lists:delete(B, Lru0),
    S#st{
        dynamic = [B | Lru1],
        symbolic = RVal
    };

next_state(#st{dynamic = []} = S, RVal, {call, _, pop, [_]}) ->
    S#st{
        symbolic = {pop, RVal}
    };

next_state(#st{} = S, RVal, {call, erlang, element, _}) ->
    S#st{
        symbolic = RVal
    };

next_state(#st{dynamic = Lru0} = S, RVal, {call, _, pop, [_]}) ->
    [_ | Lru1] = lists:reverse(Lru0),
    S#st{
        dynamic = lists:reverse(Lru1),
        symbolic = {pop, RVal}
    };

next_state(#st{dynamic = Lru0} = S, RVal, {call, _, update, [B, _]}) ->
    Lru1 = case lists:member(B, Lru0) of
        true ->
            [B | lists:delete(B, Lru0)];
        false ->
            Lru0
    end,
    S#st{
        dynamic = Lru1,
        symbolic = RVal
    };

next_state(S, _RVal, {call, _, to_list, [_]}) ->
    S.


postcondition(#st{dynamic = Lru}, {call, _, push, [B, _]}, Result) ->
    ?TEST_MOD:to_list(Result) == [B | Lru];

postcondition(#st{dynamic = []}, {call, _, pop, _}, {Item, Result}) ->
    Item == undefined andalso ?TEST_MOD:to_list(Result) == [];

postcondition(#st{dynamic = Lru}, {call, _, pop, _}, {Item, Result}) ->
    ExpectedItem = lists:last(Lru),
    ExpectedList = lists:reverse(tl(lists:reverse(Lru))),
    ExpectedItem == Item andalso ?TEST_MOD:to_list(Result) == ExpectedList;

postcondition(_S, {call, erlang, element, _}, _Result) ->
    true;

postcondition(#st{dynamic = Lru}, {call, _, update, [B, _]}, Result) ->
    case lists:member(B, Lru) of
        true ->
            ?TEST_MOD:to_list(Result) == [B | lists:delete(B, Lru)];
        false ->
            ?TEST_MOD:to_list(Result) == Lru
    end;

postcondition(#st{dynamic = Lru}, {call, _, to_list, _}, Result) ->
    Result == Lru.
