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
%%% @doc Simple load-test of adviserl application.
%%%
%%% @end
-module(load).

% ~~ Declaration: API

-export([
    start/0
]).


%%% @doc  Run all the test set.
%%% @spec () -> ok
%%% @end
start() ->
    init(),
    %        Mode  UserNumber, ItemNumber, RatingNumberPerUser, MaxRatingValue
    test_all(sync,       1000,       1000,                   5,             10),
    test_all(sync,      10000,       1000,                   5,             10),
    %
    test_all(async,      1000,       1000,                   5,             10),
    test_all(async,     10000,      10000,                   5,             10),
    test_all(async,    100000,     100000,                   5,             10),
    close(),
    ok.


% ~~ Implementation: Internal

init() ->
    application:start(sasl).

test_all(Mode, UserNumber, ItemNumber, RatingNumberPerUser, MaxRatingValue) ->
    application:start(adviserl),
    io:format(
        "== testing for ~w users, ~w items, ~w ratings per user~n",
        [UserNumber, ItemNumber, RatingNumberPerUser - 1]
    ),
    case Mode of
        sync ->
            sync_rating_test(UserNumber, ItemNumber, RatingNumberPerUser, MaxRatingValue);
        async ->
            async_rating_test(UserNumber, ItemNumber, RatingNumberPerUser, MaxRatingValue)
    end,
    recommend_all_test(UserNumber, MaxRatingValue),
    io:format("~n", []),
    application:stop(adviserl),
    application:stop(mnesia),
    ok.

sync_rating_test(UserNumber, ItemNumber, RatingNumberPerUser, MaxRatingValue) ->
    io:format("=> Adding data while updating predictions~n", []),
    D0 = erlang:now(),
    adv_util:for_seq(
        fun(UserID) ->
            adv_util:for_seq(
                fun(_RatingN) ->
                    adviserl:rate_id(
                        UserID,
                        random:uniform(ItemNumber),
                        {random:uniform(MaxRatingValue), nodata}
                    )
                end,
                1,
                RatingNumberPerUser
            )
        end,
        1,
        UserNumber
    ),
    io:format("took ~w us~n", [timer:now_diff(erlang:now(), D0)]).

async_rating_test(UserNumber, ItemNumber, RatingNumberPerUser, MaxRatingValue) ->
    io:format("=> Adding data~n", []),
    T0 = erlang:now(),
    adv_util:for_seq(
        fun(UserID) ->
            adv_util:for_seq(
                fun(_RatingN) ->
                    adviserl:async_rate_id(
                        UserID,
                        random:uniform(ItemNumber),
                        {random:uniform(MaxRatingValue), nodata}
                    )
                end,
                1,
                RatingNumberPerUser
            )
        end,
        1,
        UserNumber
    ),
    io:format("took ~w us~n", [timer:now_diff(erlang:now(), T0)]),
    io:format("=> Rebuild prediction data~n", []),
    T1 = erlang:now(),
    adv_predictions:init(),
    io:format("took ~w us~n", [timer:now_diff(erlang:now(), T1)]),
    ok.
    
recommend_all_test(UserNumber, N) ->
    adv_util:for_seq(
        fun(_N) ->
            UserN = random:uniform(UserNumber),
            io:format("=> Prediction for user ~w:~n", [UserN]),
            {T, _V} = timer:tc(adviserl, recommend_all, [UserN, [{no_key_lookup,true}]]),
            io:format("took ~w us~n", [T])
        end,
        1,
        N
    ).

close() ->
    ok.
