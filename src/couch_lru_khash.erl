% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_lru_khash).


-export([
    new/0,
    push/2,
    pop/1,
    update/2,

    % Test functions
    to_list/1
]).


-record(node, {
    dbname,
    prev,
    next
}).


new() ->
    {ok, Hash} = khash:new(),
    {Hash, undefined, undefined}.


push(DbName, T0) when is_binary(DbName) ->
    {Hash, Head, Tail} = remove(DbName, T0),
    case {Head, Tail} of
        {undefined, undefined} ->
            % Empty LRU
            ok = add_node(Hash, #node{dbname = DbName}),
            {Hash, DbName, DbName};
        {Head, Head} ->
            % Single element LRU
            ok = add_node(Hash, #node{dbname = DbName, next = Head}),
            ok = set_prev(Hash, Head, DbName),
            {Hash, DbName, Head};
        {Head, Tail} ->
            ok = add_node(Hash, #node{dbname = DbName, next = Head}),
            ok = set_prev(Hash, Head, DbName),
            {Hash, DbName, Tail}
    end.


pop({_Hash, undefined, undefined} = T0) ->
    {undefined, T0};
pop({_Hash, _Head, Tail} = T0) when is_binary(Tail) ->
    {Tail, remove(Tail, T0)}.


update(DbName, {Hash, _, _} = T0) when is_binary(DbName) ->
    case get_node(Hash, DbName) of
        undefined ->
            % We closed this database beore processing the update. Ignore
            T0;
        _ ->
            push(DbName, T0)
    end.


to_list({_, undefined, undefined}) ->
    [];
to_list({_Hash, Head, Head}) when is_binary(Head) ->
    [Head];
to_list({Hash, Head, Tail}) when is_binary(Head), is_binary(Tail) ->
    to_list(Hash, Head, []).


to_list(Hash, undefined, Nodes) ->
    true = length(Nodes) == khash:size(Hash),
    lists:reverse(Nodes);
to_list(Hash, Curr, Nodes) when is_binary(Curr) ->
    false = lists:member(Curr, Nodes),
    Node = get_node(Hash, Curr),
    to_list(Hash, Node#node.next, [Curr | Nodes]).


% Internal

remove(DbName, {Hash, Head, Tail}) when is_binary(DbName) ->
    case get_node(Hash, DbName) of
        undefined ->
            {Hash, Head, Tail};
        Node ->
            ok = set_next(Hash, Node#node.prev, Node#node.next),
            ok = set_prev(Hash, Node#node.next, Node#node.prev),
            ok = del_node(Hash, Node),
            NewHead = if DbName /= Head -> Head; true ->
                Node#node.next
            end,
            NewTail = if DbName /= Tail -> Tail; true ->
                Node#node.prev
            end,
            {Hash, NewHead, NewTail}
    end.


get_node(_Hash, #node{} = Node) ->
    Node;
get_node(Hash, DbName) ->
    khash:get(Hash, DbName).


add_node(Hash, #node{} = Node) ->
    undefined = khash:get(Hash, Node#node.dbname),
    khash:put(Hash, Node#node.dbname, Node),
    ok.


del_node(Hash, #node{} = Node) ->
    {value, _} = khash:lookup(Hash, Node#node.dbname),
    khash:del(Hash, Node#node.dbname).


set_next(_, undefined, _) ->
    ok;
set_next(Hash, #node{dbname = DbName}, Next) ->
    set_next(Hash, DbName, Next);
set_next(Hash, Node, #node{dbname = Next}) ->
    set_next(Hash, Node, Next);
set_next(Hash, NodeDbName, NextDbName) when is_binary(NodeDbName) ->
    Node = get_node(Hash, NodeDbName),
    NewNode = Node#node{next = NextDbName},
    khash:put(Hash, NodeDbName, NewNode).


set_prev(_, undefined, _) ->
    ok;
set_prev(Hash, #node{dbname = DbName}, Prev) when is_binary(DbName) ->
    set_prev(Hash, DbName, Prev);
set_prev(Hash, Node, #node{dbname = DbName}) ->
    set_prev(Hash, Node, DbName);
set_prev(Hash, NodeDbName, PrevDbName) when is_binary(NodeDbName) ->
    Node = get_node(Hash, NodeDbName),
    NewNode = Node#node{prev = PrevDbName},
    khash:put(Hash, NodeDbName, NewNode).
