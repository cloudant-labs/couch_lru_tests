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

-module(couch_lru_dict).

-export([
    new/0,
    push/2,
    pop/1,
    update/2,

    to_list/1
]).


new() ->
    {gb_trees:empty(), dict:new()}.


push(DbName, {Tree0, Dict0}) ->
    Lru = couch_util:unique_monotonic_integer(),
    {gb_trees:insert(Lru, DbName, Tree0), dict:store(DbName, Lru, Dict0)}.


pop({Tree0, Dict0}) ->
    case gb_trees:size(Tree0) of
        Size when Size > 0 ->
            {_Lru, DbName, Tree1} = gb_trees:take_largest(Tree0),
            Dict1 = dict:erase(DbName, Dict0),
            {DbName, {Tree1, Dict1}};
        0 ->
            {undefined, {Tree0, Dict0}}
    end.


update(DbName, {Tree0, Dict0}) ->
    case dict:find(DbName, Dict0) of
    {ok, Old} ->
        New = couch_util:unique_monotonic_integer(),
        Tree = gb_trees:insert(New, DbName, gb_trees:delete(Old, Tree0)),
        Dict = dict:store(DbName, New, Dict0),
        {Tree, Dict};
    error ->
        % We closed this database before processing the update.  Ignore
        {Tree0, Dict0}
    end.


to_list({Tree0, _}) ->
    to_list(gb_trees:next(gb_trees:iterator(Tree0)), []).


to_list(none, Acc) ->
    lists:reverse(Acc);
to_list({_Lru, DbName, Iter}, Acc) ->
    to_list(gb_trees:next(Iter), [DbName | Acc]).
