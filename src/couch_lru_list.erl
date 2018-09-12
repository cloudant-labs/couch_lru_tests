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

-module(couch_lru_list).

-export([
    new/0,
    push/2,
    pop/1,
    update/2,

    to_list/1
]).


new() ->
    [].


push(DbName, Lru) ->
    [DbName | lists:delete(DbName, Lru)].


pop([]) ->
    {undefined, []};
pop(Lru) ->
    [Item | Rest] = lists:reverse(Lru),
    {Item, lists:reverse(Rest)}.


update(DbName, Lru) ->
    case lists:member(DbName, Lru) of
        true ->
            push(DbName, Lru);
        false ->
            Lru
    end.


to_list(Lru) ->
    Lru.
