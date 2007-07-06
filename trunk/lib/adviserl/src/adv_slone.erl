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
%%% @doc Slope-One covisitation recommender callback for adviserl.
%%% @reference <a href="http://www.daniel-lemire.com/fr/abstracts/SDM2005.html">Daniel Lemire publication, 2005.</a>
%%%
%%% @end
-module(adv_slone).

% ~~ Declaration: API

% callbacks for adv_items
-export([
    create_state/1,
    load_ratings/0,
    update_rating/5,
    predict_all/3,
    print_debug/1
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").
-include("include/adv_mat.hrl").


% ~~ Implementation: API

%%% @doc  Create data structure: skew matrix containing all cross references.
%%% @spec ([Option]) -> SkewMatrix
%%% @private
%%% @end
create_state(_Options) ->
    create_skewmatrix().

%%% @doc  Traverse all available ratings to populate the items matrix.
%%% @spec () -> SkewMatrix
%%% @private
%%% @end
load_ratings() ->
    %TODO this may be parallelized...
    adv_ratings:fold_sources(
        fun(SourceID, SourceRatings, AccMatrix) ->
            %?DEBUG("Fold sources: source=~w", [SourceID]),
            eval_ratings_items(
                fun update_items_add/3,
                SourceRatings,
                AccMatrix
            )
        end,
        create_skewmatrix()
    ).

%%% @doc  Update a rating: change matrix state to reflect this update.
%%% @spec (sourceID(), itemID(), rating(), ratings()|undefined, Matrix) -> Matrix
%%% @private
%%% @todo Use OldRating to not recompute all ratings items but only
%%% those involving ItemID
%%% @end
update_rating(SourceID, ItemID, _NewRating, OldRatings, Matrix) ->
    % revert rating influence
    RevertedMatrix = case OldRatings of
        undefined ->
            Matrix;
        Any ->
            eval_ratings_items(
                fun update_items_del/3,
                OldRatings,
                Matrix
            )
    end,
    % compute new influences
    case adv_ratings:get_ratings(SourceID) of
        undefined ->
            ?ERROR(
                "Updating non existing rating (sourceID=~w itemID=~w)",
                [SourceID, ItemID],
                {error, "Bad usage of adv_items:update_rating/5"}
            );
        SourceRatings ->
            eval_ratings_items(
                fun update_items_add/3,
                SourceRatings,
                RevertedMatrix
            )
    end.

%%% @doc  Given a set of ratings and a items matrix, return a list of
%%% key-value pair, giving the prediction value for each key.
%%% @see  adviserl:recommend_all/2
%%% @spec (ratings(), ItemsMatrix, [Option]) -> predictions()
%%% @private
%%% @end
predict_all(SourceRatings, ItemsMatrix, Options) ->
    % hopefully this respect the weighted slope one algorithm
    %fprof:trace(start),
    Result = adv_mat_sm:map_per_partial_row(
        fun(PartialRow=#mat_row{line=LItem}) ->
            {Freq, Dev} = adv_ratings:fold_ratings(
                fun(CItem, _CRating={CVal,_CData}, Acc={FreqAcc, DevAcc}) ->
                    case LItem == CItem of
                        true ->
                            Acc;
                        _ ->
                            case LItem > CItem of
                                true ->
                                    case adv_mat_sm:get_partial_row_value(CItem, PartialRow) of
                                        {ok, {RFreq, RDev}} ->
                                            {
                                                FreqAcc + RFreq,
                                                DevAcc + RFreq * (RDev + CVal)
                                            };
                                        _ ->
                                            Acc
                                    end;
                                _ ->
                                    case adv_mat_sm:get(LItem, CItem, ItemsMatrix) of
                                        {ok, {ARFreq, ARDev}} ->
                                            {
                                                FreqAcc + ARFreq,
                                                DevAcc + ARFreq * (ARDev + CVal)
                                            };
                                        error ->
                                            Acc
                                    end
                            end
                    end
                end,
                {0, 0},
                SourceRatings
            ),
            {Freq, Dev}
        end,
        ItemsMatrix
    ),
    %fprof:trace(stop),
    format_prediction_result(Result, SourceRatings, Options).

%%% @doc  Print the full matrix on debug streams.
%%% @spec (Matrix) -> ok
%%% @end
print_debug(Matrix) ->
    adv_mat_sm:fold_per_partial_row(
        fun(Row, Acc1) ->
            Line = adv_mat_sm:fold_per_partial_row(
                fun(Col, Acc2) ->
                    LN = Row#mat_row.line,
                    CN = Col#mat_row.line,
                    case adv_mat_sm:get(LN, CN, Matrix) of
                        error ->
                            io_lib:format("~s ~5B:       ", [Acc2, CN]);
                        {ok, {F,D}} ->
                            io_lib:format("~s ~5B:~3B,~3B", [Acc2, CN, F, D])
                    end
                end,
                io_lib:format("item ~5B: ", [Row#mat_row.line]),
                Matrix
            ),
            ?DEBUG("~s", [Line]),
            Acc1
        end,
        ok,
        Matrix
    ).


% ~~ Implementation: Internal

%%% @doc  Create the datastructure to contain frequencies and deviations.
%%% @spec () -> SkewMatrix
%%% @private
%%% @end
create_skewmatrix() ->
    SymFun = fun({Freq,Dev}) -> {Freq,-Dev} end,
    adv_mat_sm:new(SymFun).

%%% @doc  Given one set of ratings, add the items to matrix using the
%%% input function U to evaluate the weight. U take two related ratings and
%%% the previous value of items, and return the new value of items.
%%% @see  update_items_add/3
%%% @spec (Updator::(rating(), rating(), integer())->integer(), ratings(), ItemsMatrix) -> ItemsMatrix
%%% @private
%%% @end
eval_ratings_items(ItemsUpdator, SourceRatings, ItemsMatrix) ->
    NewItemsMatrix = adv_ratings:fold_ratings(
        fun(Item1, Rating1, Mat) ->
            %?DEBUG("Fold ratings: item=~w rating=~w", [Item1, Rating1]),
            % get items for this item
            Row1 = adv_mat_sm:get_partial_row(Item1, Mat),
            % compute new items with all other items (using skew property)
            NewRow1 = adv_ratings:fold_ratings(
                fun(Item2, Rating2, Row) ->
                    case Item2 < Item1 of
                        true ->
                            %?DEBUG("Fold ratings: item=~w rating=~w: add itemref=~w ratingref=~w", [Item1, Rating1, Item2, Rating2]),
                            adv_mat_sm:update_partial_row_value(
                                Item2,
                                fun(Items) ->
                                    ItemsUpdator(Rating1, Rating2, Items)
                                end,
                                ItemsUpdator(Rating1, Rating2, undefined),
                                Row
                            );
                        _ ->
                            Row
                    end
                end,
                Row1,
                SourceRatings
            ),
            % store new items for this Item
            NewMat = adv_mat_sm:set_partial_row(NewRow1, Mat),
            NewMat
        end,
        ItemsMatrix,
        SourceRatings
    ),
    NewItemsMatrix.

%%% @doc  Given two related ratings and the previous value of items, return
%%% the new value of items.
%%% @spec (rating(), rating(), ItemsValue) -> ItemsValue
%%% @private
%%% @end
update_items_add({Val1, _Data1}, {Val2, _Data2}, undefined) ->
    {1, Val1 - Val2};
update_items_add({Val1, _Data1}, {Val2, _Data2}, {Freq, Dev}) ->
    {Freq + 1, Dev + (Val1 - Val2)}.

%%% @doc  Does the opposite of update_items_add/3
%%% @spec (rating(), rating(), ItemsValue) -> ItemsValue
%%% @private
%%% @end
update_items_del({Val1, _Data1}, {Val2, _Data2}, undefined) ->
    {1, Val2 - Val1};
update_items_del({Val1, _Data1}, {Val2, _Data2}, {Freq, Dev}) ->
    {Freq - 1, Dev + (Val2 - Val1)}.

%%% @doc  Use options to format the predictions results.
%%% @spec ([{itemID(), {Freq::integer(), Dev::integer()}}], [rating()], [Option]) -> predictions()
%%% @see  adviserl:recommend_all/2
%%% @private
%%% @end
format_prediction_result(Result0, Ratings, Options) ->
    % removing 'false' or 'bad' predictions
    Result1 = case proplists:get_bool(no_strict, Options) of
        false ->
            lists:filter(
                fun
                    ({_ItemID, {0,_Dev}}) ->
                        false;
                    (_) ->
                        true
                end,
                Result0
            );
        _ ->
            Result0
    end,
    % removing predictions for known ratings
    {Result2, SourceRatingNumber} = adv_ratings:fold_ratings(
        fun(ItemID, ItemRating, {Predictions, RatingNumber}) ->
                %io:format("removing ~p: ~p~n", [ItemID, lists:keymember(ItemID, 1, Predictions)]),
                {lists:keydelete(ItemID, 1, Predictions), RatingNumber + 1}
        end,
        {Result1, 0},
        Ratings
    ),
    % compute float value as prediction coefficients
    Coef = float(SourceRatingNumber),
    Result3 = lists:map(
        fun({ItemID, {Freq,Dev}}) ->
            {ItemID, float(Freq * Dev) / Coef}
        end,
        Result2
    ),
    % sort by decreasing prediction values
    Result4 = case proplists:get_bool(no_sorted, Options) of
        false ->
            lists:reverse(lists:keysort(2, Result3));
        _ ->
            Result3
    end,
    % finally
    Result4.

% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
