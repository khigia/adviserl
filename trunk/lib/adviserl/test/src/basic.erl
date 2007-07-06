%===========================================================================
% This file is part of adviserl.
%
% adviserl is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 3 of the License, or
% (at your option) any later version.
%
% adviserl is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with adviserl; if not, write to the Free Software
% Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%===========================================================================
%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @copyright 2007 Affle Pvt. Ltd.
%%% @doc Basic test of adviserl application.
%%%
%%% @end
-module(basic).

% ~~ Declaration: API

-export([
    start/0
]).


%%% @doc  Run all the test set.
%%% @spec () -> ok
%%% @end
start() ->
    init(),
    simple_test(),
    update_test(),
    close(),
    ok.


% ~~ Implementation: Internal

banner(0, Text) ->
    io:format("~n==== ~s ====~n", [Text]);
banner(1, Text) ->
    io:format("~n=> ~s~n", [Text]).
    
init() ->
    application:start(sasl),
    application:start(adviserl).

simple_test() ->
    banner(0, "Simple test"),
    banner(1, "set few ratings"),
    lists:foreach(
        fun({SourceID, ItemID, Rating}) ->
            io:format("source ~w rating item ~w by ~w~n", [SourceID, ItemID, Rating]),
            adviserl:rate(SourceID, ItemID, Rating)
        end,
        [
            {1,2,{3,rating_data}},
            {1,4,{5,rating_data}},
            {2,2,{1,rating_data}},
            {2,5,{8,rating_data}},
            {3,4,{3,rating_data}},
            {3,5,{2,rating_data}},
            {3,12,{2,rating_data}}
        ]
    ),
    banner(1, "rating matrix"),
    adv_ratings:print_debug(),
    banner(1, "items matrix"),
    adv_items:print_debug(),
    banner(1, "prediction examples"),
    lists:foreach(
        fun(Source) ->
            io:format("recommend_all(~w): ~w~n", [
                Source,
                adviserl:recommend_all(Source)
            ])
        end,
        [
            1,
            2,
            3,
            4,
            [],
            [{3,5}],
            [{2,5}],
            [{4,5}],
            [{2,5},{4,5}]
        ]
    ).

update_test() ->
    banner(0, "Update test"),
    banner(1, "rating matrix"),
    adv_ratings:print_debug(),
    banner(1, "items matrix"),
    adv_items:print_debug(),
    banner(1, "update few ratings"),
    lists:foreach(
        fun({SourceID, ItemID, Rating}) ->
            io:format("source ~w rate item ~w by ~w~n", [SourceID, ItemID, Rating]),
            adviserl:rate(SourceID, ItemID, Rating)
        end,
        [
            {1,2,{7,rating_data}},
            {3,4,{1,rating_data}}
        ]
    ),
    banner(1, "rating matrix"),
    adv_ratings:print_debug(),
    banner(1, "items matrix"),
    adv_items:print_debug(),
    banner(1, "update few ratings"),
    lists:foreach(
        fun({SourceID, ItemID, Rating}) ->
            io:format("source ~w rate item ~w by ~w~n", [SourceID, ItemID, Rating]),
            adviserl:rate(SourceID, ItemID, Rating)
        end,
        [
            {1,2,{3,rating_data}},
            {3,4,{3,rating_data}}
        ]
    ),
    banner(1, "rating matrix"),
    adv_ratings:print_debug(),
    banner(1, "items matrix"),
    adv_items:print_debug(),
    ok.
    
close() ->
    application:stop(adviserl).
