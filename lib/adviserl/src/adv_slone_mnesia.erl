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
%%% @doc Slope-One using mnesia table.
%%% @reference <a href="http://www.daniel-lemire.com/fr/abstracts/SDM2005.html">Daniel Lemire publication, 2005.</a>
%%%
%%% @end
-module(adv_slone_mnesia).


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

-include_lib("stdlib/include/qlc.hrl").

-include("include/adviserl.hrl").

-record(st, {
    table
}).

-record(slnscore, {
    row_item,
    col_item,
    freq,
    diff
}).


% ~~ Implementation: API
%empty


% ~~ Implementation: Behaviour callbacks

init([TableName]) ->
    State = #st{
        table = init_table(TableName)
    },
    {ok, State}.

handle_call({load_file, _File, _Options}, _From, State) ->
    % mnesia automaticaly restore dumps
    {reply, ok, State};

handle_call({save_file, _File, _Options}, _From, State = #st{table = Tbl}) ->
    % simple dump
    {reply, mnesia:dump_tables([Tbl]), State};

handle_call(init, _From, State = #st{table = Tbl}) ->
    R = init_predictions(Tbl),
    {reply, R, State};

handle_call(
    {update_rating, SourceID, ItemID, NewRating, OldRatings},
    _From,
    State = #st{table = Table}
) ->
    ok = update_rating(Table, SourceID, ItemID, NewRating, OldRatings),
    {reply, ok, State};

handle_call(
    {predict_all, Ratings, Options},
    _From,
    State = #st{table = Table}
) ->
    Prediction = predict_all(Table, Ratings, Options),
    {reply, Prediction, State};

handle_call(
    print_debug,
    _From,
    State = #st{table = Table}
) ->
    print_debug(Table),
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

%%% @spec init_table(TableName::atom()) -> TableName::atom()
%%% @private
init_table(TableName) ->
    %TODO should not try to create it at each start (distributed app)
    Create = fun() ->
        Status = mnesia:create_table(TableName, [
            % distribution properties
            {ram_copies, [node()]},
            % table(data) properties
            {record_name, slnscore},
            {type, bag},
            {attributes, record_info(fields, slnscore)},
            {index, [col_item]}
        ]),
        mnesia:add_table_index(TableName, col_item),
        Status
    end,
    Prepare = fun() ->
        ok = mnesia:wait_for_tables([TableName], 20000),
        TableName
    end,
    case Create() of
        {atomic,ok} ->
            ?DEBUG("table '~w' created", [TableName]),
            Prepare();
        {aborted,{already_exists,TableName}} ->
            ?DEBUG("using existing table '~w'", [TableName]),
            Prepare();
        Status ->
            ?ERROR("cannot create table '~w'", [TableName], Status),
            undefined
    end.

%%% @spec init_predictions(Table::atom()) -> ok
%%% @doc  Clean whole table then traverse all ratings to re-populate it.
%%% @private
init_predictions(Table) ->
    % delete the whole table content
    {atomic, ok} = mnesia:clear_table(Table),
    % read all ratings to (re)populate the table
    {atomic, _} = mnesia:transaction(fun() ->
        mnesia:write_lock_table(Table),
        adv_ratings:fold_sources(
            fun(_SourceID, SourceRatings, Acc) ->
                %?DEBUG("Fold sources: source=~w", [SourceID]),
                % Following code is equivalent but less efficient:
                %     eval_ratings_items(
                %         Table,
                %         fun update_items_add/3,
                %         SourceRatings
                %     ),
                adv_ratings:fold_ratings(
                    fun(Item1, Rating1, ok) ->
                        % new crossref with all other items (skew matrix)
                        adv_ratings:fold_ratings(
                            fun(Item2, Rating2, ok) ->
                                case Item2 > Item1 of
                                    true ->
                                        Value = update_items_add(
                                            Rating1,
                                            Rating2,
                                            new_slone_score(Item1, Item2)
                                        ),
                                        mnesia:dirty_write(Table, Value);
                                    _ ->
                                        ok
                                end
                            end,
                            ok,
                            SourceRatings
                        )
                    end,
                    ok,
                    SourceRatings
                ),
                Acc
            end,
            ok
        )
    end),
    ok.


%%% @spec eval_ratings_items(Table, Updater, ratings()) -> ok
%%%     Updater = (rating(), rating(), integer()) -> integer()
%%% @doc  Given one set of ratings, add the items to table using the
%%% input function U to evaluate the weight. U take two related ratings and
%%% the previous value of items, and return the new value of items.
%%% @see  update_items_add/3
%%% @private
eval_ratings_items(Table, ItemsUpdator, SourceRatings) ->
    adv_ratings:fold_ratings(
        fun(Item1, Rating1, ok) ->
            % new crossref with all other items (using skew property)
            adv_ratings:fold_ratings(
                fun(Item2, Rating2, ok) ->
                    case Item2 > Item1 of
                        true ->
                            update_record(
                                Table,
                                Item1,
                                Item2,
                                fun(SlnScore) ->
                                    ItemsUpdator(Rating1, Rating2, SlnScore)
                                end,
                                fun() ->
                                    ItemsUpdator(Rating1, Rating2, new_slone_score(Item1,Item2))
                                end
                            );
                        _ ->
                            ok
                    end
                end,
                ok,
                SourceRatings
            )
        end,
        ok,
        SourceRatings
    ).

%% @spec new_slone_score(Row::itemID(), Col::itemID()) -> Record
%%     Record = {slnscore, Row, Col, 0, 0}
%% @private
new_slone_score(Row, Col) ->
    #slnscore{
        row_item = Row,
        col_item = Col,
        freq = 0,
        diff = 0
    }.

%%% @spec (rating(), rating(), SloneScore) -> NewSloneScore
%%% @doc  Given two related ratings and the previous crossref score, return
%%% the new crossref score.
%%% @private
update_items_add({Val1, _Data1}, {Val2, _Data2}, Score = #slnscore{freq=Freq, diff=Dev}) ->
    Score#slnscore{freq = Freq + 1, diff = Dev + (Val1 - Val2)}.

%%% @spec (rating(), rating(), SloneScore) -> NewSloneScore
%%% @doc  Does the opposite of update_items_add/3
%%% @private
update_items_del({Val1, _Data1}, {Val2, _Data2}, Score = #slnscore{freq = Freq, diff = Dev}) ->
    Score#slnscore{freq = Freq - 1, diff = Dev + (Val2 - Val1)}.

%% @spec update_rating(Table, sourceID(), itemID(), NewRating, OldRatings) -> ok
%%     NewRating = rating()
%%     OldRatings = ratings()|undefined
%%% @doc  Update a rating: change matrix state to reflect this update.
%%% @private
update_rating(Table, SourceID, ItemID, _NewRating, OldRatings) ->
    % revert rating influence
    ok = case OldRatings of
        undefined ->
            ok;
        _Any ->
            eval_ratings_items(
                Table,
                fun update_items_del/3,
                OldRatings
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
                Table,
                fun update_items_add/3,
                SourceRatings
            )
    end.


%%% @spec predict_all(Table, ratings(), [Option]) -> predictions()
%%% @doc  Given a set of ratings and a items matrix, return a list of
%%% key-value pair, giving the prediction value for each key.
%%% @see  adviserl:recommend_all/2
%%% @private
predict_all(Table, SourceRatings, Options) ->
    % hopefully this respect the weighted slope one algorithm
    %fprof:trace(start),
    Result = predict_all_impl2(Table, SourceRatings),
    %fprof:trace(stop),
    format_prediction_result(Result, SourceRatings, Options).


%%% @private
predict_all_impl2(Table, SourceRatings) ->
%predict_all_impl1(Table, SourceRatings) ->
%    % this implementation is not efficient
%    {atomic, Result} = mnesia:transaction(fun() -> adv_items:fold(
%        % performance problem: folding item, we are querying the predictions
%        % table for all item even those without crossref, and this for each
%        % rating of source
%        fun(RowItem, _Key, _Data, ResultAcc) ->
%            Score = adv_ratings:fold_ratings(
%                fun
%                    (ColItem, _Cell, Acc) when ColItem =:= RowItem ->
%                        Acc; % ignore diagonal
%                    (ColItem, {CVal,_CData}, Acc={FreqAcc, DevAcc}) ->
%                        case get_cell_inmnesia(Table, RowItem, ColItem) of
%                            {Freq, Dev} ->
%                                {
%                                    FreqAcc + Freq,
%                                    DevAcc + Freq * (Dev + CVal)
%                                };
%                            _ ->
%                                Acc
%                        end
%                end,
%                {0, 0}, % {freq, diff}
%                SourceRatings
%            ),
%            [{RowItem,Score}|ResultAcc]
%        end,
%        []
%    ) end),
%    Result.
    Dict = adv_ratings:fold_ratings(
        fun(ID, {CVal,_CData}, Acc) ->
            %following is get_col(Table, Col) + adding CVal
            {atomic, Result} = mnesia:transaction(fun() ->
                QH1 = qlc:q([
                    {
                        R1#slnscore.col_item, [{
                            R1#slnscore.freq,
                            - R1#slnscore.diff,
                            CVal
                        }]
                    } ||
                    R1 <- mnesia:table(Table),
                    R1#slnscore.row_item =:= ID
                ]),
                QH2 = qlc:q([
                    {
                        R2#slnscore.row_item, [{
                            R2#slnscore.freq,
                            R2#slnscore.diff,
                            CVal
                        }]
                    } ||
                    R2 <- mnesia:table(Table),
                    R2#slnscore.row_item < ID, % hint for query index
                    R2#slnscore.col_item =:= ID
                ]),
                QR1 = qlc:e(QH1),
                QR2 = qlc:e(QH2),
                QR1 ++ QR2
                %qlc:e(qlc:append(QH1,QH2)) failed ...?
            end),
            NewDict = dict:from_list(Result),
            dict:merge(fun(_K,V1,V2) -> V1 ++ V2 end, NewDict, Acc)
        end,
        dict:new(),
        SourceRatings
    ),
    L = dict:to_list(Dict),
    lists:map(
        fun({ItemID, Scores}) ->
            {
                ItemID,
                lists:foldl(
                    fun({F,D,V}, {FAcc,DAcc}) ->
                        {FAcc + F, DAcc + F * (D + V)}
                    end,
                    {0,0},
                    Scores
                )
            }
        end,
        L
    ).

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

%%% @spec print_debug(Table) -> ok
%%% @doc  Print the full matrix on debug streams.
print_debug(Table) ->
    Items = adv_items:fold(fun(ID, _Key, _Data, Acc) -> [ID|Acc] end, []),
    lists:foldl(
        fun(RowItem, Acc) ->
            Line = lists:foldl(
                fun
                    (ColItem, CurLine) when ColItem < RowItem ->
                        case get_cell(Table, RowItem, ColItem) of
                            {Freq, Diff} ->
                                io_lib:format(
                                    "~s ~5B:~3B,~3B",
                                    [CurLine, ColItem, Freq, Diff]
                                );
                            _Any ->
                                io_lib:format(
                                    "~s ~5B:       ",
                                    [CurLine, ColItem]
                                )
                        end;
                    (_ColItem, CurLine) ->
                        CurLine
                end,
                io_lib:format("item ~5B: ", [RowItem]),
                Items
            ),
            ?DEBUG("~s", [Line]),
            Acc
        end,
        ok,
        Items
    ).


%dirty_update_record(Table, ItemRow, ItemCol, Updater, Default) ->
%    F = fun() ->
%        dirty_update_record_inmnesia(
%            Table, ItemRow, ItemCol, Updater, Default
%        )
%    end,
%    mnesia:async_dirty(F),
%    ok.
%
%dirty_update_record_inmnesia(Table, ItemRow, ItemCol, Updater, Default)
%    when
%        ItemCol > ItemRow % symmetric matrix
%    ->
%    %Wild = mnesia:table_info(Table, wild_pattern),
%    QR = mnesia:dirty_match_object(
%        Table,
%        #slnscore{row_item = ItemRow, col_item = ItemCol, _ = '_'}
%    ),
%    case QR of
%        [] ->
%            % no previsous value, use default
%            mnesia:dirty_write(Table, Default());
%        [Record] ->
%            NewRecord = Updater(Record),
%            ok = mnesia:dirty_delete_object(Table, Record),
%            mnesia:dirty_write(Table, NewRecord);
%        Any ->
%            ?ERROR(
%                "DB error or more than one result for cell (~B,~B)",
%                [ItemRow,ItemCol],
%                Any
%            )
%    end.

update_record(Table, ItemRow, ItemCol, Updater, Default) ->
    F = fun() ->
        update_record_inmnesia(
            Table, ItemRow, ItemCol, Updater, Default
        )
    end,
    {atomic, _} = mnesia:transaction(F),
    ok.

update_record_inmnesia(Table, ItemRow, ItemCol, Updater, Default)
    when
        ItemCol > ItemRow % symmetric matrix
    ->
    %Wild = mnesia:table_info(Table, wild_pattern),
    QR = mnesia:match_object(
        Table,
        #slnscore{row_item = ItemRow, col_item = ItemCol, _ = '_'},
        sticky_write
    ),
    case QR of
        [] ->
            % no previsous value, use default
            mnesia:write(Table, Default(), write);
        [Record] ->
            NewRecord = Updater(Record),
            ok = mnesia:delete_object(Table, Record, sticky_write),
            mnesia:write(Table, NewRecord, write);
        Any ->
            ?ERROR(
                "DB error or more than one result for cell (~B,~B)",
                [ItemRow,ItemCol],
                Any
            )
    end.

%%@spec get_cell(Table, ItemRow, ItemCol) -> {Freq, Diff} | undefined
get_cell(Table, ItemRow, ItemCol) ->
    {atomic, Reply} = mnesia:transaction(
        fun() -> get_cell_inmnesia(Table, ItemRow, ItemCol) end
    ),
    Reply.

get_cell_inmnesia(Table, ItemRow, ItemCol) when ItemCol > ItemRow ->
    QH = qlc:q([R ||
        R <- mnesia:table(Table),
        R#slnscore.row_item =:= ItemRow,
        R#slnscore.col_item =:= ItemCol
    ]),
    case qlc:e(QH) of
        [] ->
            undefined;
        [#slnscore{freq = Freq, diff = Diff}] ->
            {Freq, Diff};
        Any ->
            ?ERROR(
                "DB error or more than one result in query [~s]",
                [qlc:info(QH)],
                Any
            )
    end;
get_cell_inmnesia(Table, ItemRow, ItemCol) when ItemCol < ItemRow ->
    % symetric function
    case get_cell_inmnesia(Table, ItemCol, ItemRow) of
        {Freq, Diff} ->
            {Freq, - Diff};
        Any ->
            Any
    end.


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.

