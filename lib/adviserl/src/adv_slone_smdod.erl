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
%%% @doc Slope-One using dict-of-dict skewmatrix in memory storage.
%%% @reference <a href="http://www.daniel-lemire.com/fr/abstracts/SDM2005.html">Daniel Lemire publication, 2005.</a>
%%%
%%% @end
-module(adv_slone_smdod).


% ~~ Declaration: OTP relative
-behaviour(gen_server).
-export([
    init/1,         % (InitArgs) -> Result
    handle_call/3,  % (Request, From, State) -> Result
    handle_cast/2,  % (Request, State) -> Result
    handle_info/2,  % (Info, State) -> Result
    terminate/2,    % (Reason, State) -> term() % result is not used
    code_change/3   % (OldVsn, State, Extra) -> {ok, NewState}
]).


% ~~ Declaration: API
%empty


% ~~ Declaration: Internal

-include("include/adviserl.hrl").
-include("include/adv_mat.hrl").

-record(st, {
    matrix
}).


% ~~ Implementation: API
%empty


% ~~ Implementation: Behaviour callbacks

init(_InitArgs) ->
    State = #st{
        matrix = create_state()
    },
    {ok, State}.

handle_call(
    {load_file, File, _Options},
    _From,
    State=#st{matrix = Matrix1}
) ->
    case file:consult(File) of
        {ok, [Data]} ->
            Matrix2 = adv_mat_sm:set_internal_data(Data, Matrix1),
            {reply, ok, State#st{matrix = Matrix2}};
        {ok, [_|_]} ->
            {reply, {error, "Bad file format."}, State};
        Err1 ->
            {reply, Err1, State}
    end;

handle_call(
    {save_file, File, _Options},
    _From,
    State
) ->
    case file:open(File, [write]) of
        {ok, IODev} ->
            Matrix = State#st.matrix,
            Data = adv_mat_sm:get_internal_data(Matrix),
            io:fwrite(IODev, "~w.", [Data]),
            file:close(IODev),
            {reply, ok, State};
        Err1 ->
            {reply, Err1, State}
    end;

handle_call(
    init,
    _From,
    State = #st{}
) ->
    M = load_ratings(),
    {reply, ok, State#st{matrix = M}};

handle_call(
    {update_rating, SourceID, ItemID, NewRating, OldRatings},
    _From,
    State = #st{matrix = Data0}
) ->
    Data1 = update_rating(SourceID, ItemID, NewRating, OldRatings, Data0),
    {reply, ok, State#st{matrix = Data1}};

handle_call(
    {predict_all, Ratings, Options},
    _From,
    State = #st{matrix = Data0}
) ->
    Prediction = predict_all(Ratings, Data0, Options),
    {reply, Prediction, State};

handle_call(
    print_debug,
    _From,
    State = #st{matrix = Data0}
) ->
    print_debug(Data0),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, unknow_call_request, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

%%% @doc  Create data structure: skew matrix containing all cross references.
%%% @spec () -> SkewMatrix
%%% @private
%%% @end
create_state() ->
    create_skewmatrix().

%%% @doc  Traverse all available ratings to populate the items matrix.
%%% @spec () -> SkewMatrix
%%% @private
%%% @end
load_ratings() ->
    %TODO this may be parallelized...
    adv_ratings:fold_sources(
        fun(_SourceID, SourceRatings, AccMatrix) ->
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
        _Any ->
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
                {error, "Bad usage of adv_predictions:update_rating/5"}
            );
        SourceRatings ->
            eval_ratings_items(
                fun update_items_add/3,
                SourceRatings,
                RevertedMatrix
            )
    end.


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

%%% @doc  Use options to format the predictions results.
%%% @spec ([{itemID(), {Freq::integer(), Dev::integer()}}], [rating()], [Option]) -> predictions()
%%% @see  adviserl:recommend_all/2
%%% @private
%%% @end
format_prediction_result(Result0, Ratings, Options) ->
    % removing 'false' or 'bad' predictions
    %?DEBUG("prediction 0: ~w~n", [Result0]),
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
    %?DEBUG("prediction 1: ~w~n", [Result1]),
    % removing predictions for known ratings
    {Result2, SourceRatingNumber} = case proplists:get_bool(no_remove_known, Options) of
        false ->
            adv_ratings:fold_ratings(
                fun(ItemID, _ItemRating, {Predictions, RatingNumber}) ->
                    %io:format("removing ~p: ~p~n", [ItemID, lists:keymember(ItemID, 1, Predictions)]),
                    {lists:keydelete(ItemID, 1, Predictions), RatingNumber + 1}
                end,
                {Result1, 0},
                Ratings
            );
        _ ->
            {Result1, adv_ratings:fold_ratings(
                fun(_ItemID, _ItemRating, RatingNumber) ->
                    RatingNumber + 1
                end,
                0,
                Ratings
            )}
    end,
    %?DEBUG("prediction 2: ~w~n", [Result2]),
    % compute float value as prediction coefficients
    Coef = float(SourceRatingNumber),
    Result3 = lists:map(
        fun({ItemID, {Freq,Dev}}) ->
            {ItemID, float(Freq * Dev) / Coef}
        end,
        Result2
    ),
    %?DEBUG("prediction 3: ~w~n", [Result3]),
    % sort by decreasing prediction values
    Result4 = case proplists:get_bool(no_sorted, Options) of
        false ->
            lists:reverse(lists:keysort(2, Result3));
        _ ->
            Result3
    end,
    %?DEBUG("prediction 4: ~w~n", [Result4]),
    % finally
    Result4.

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


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.

