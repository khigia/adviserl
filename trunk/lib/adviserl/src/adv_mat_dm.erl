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
%%% @doc (Sparse) 2D Dictionary-based Matrix.
%%%
%%% Implementation is a dict of dict: fast to get one line.
%%%
%%% @end
-module(adv_mat_dm).


% ~~ Declaration: OTP relative


% ~~ Declaration: API
-export([
    new/0,
    get/3,
    set/4,
    update/5,
    get_row/2,
    set_row/2,
    get_row_value/2,
    set_row_value/3,
    update_row_value/4,
    fold_row/3,
    map_per_row/2,
    fold_per_row/3
]).

% ~~ Declaration: Internal
-define(DICT, dict).

-include("include/adv_mat.hrl").


% ~~ Implementation: API

%%% @doc  Create a new matrix.
%%% @spec () -> matrix()
%%% @end
new() ->
    ?DICT:new().

%%% @doc  Get value of matrix element.
%%% @spec (LN::integer(), CN::integer(), matrix()) -> {ok, Val}|error
%%% @end
get(LN, CN, M) when is_integer(LN), is_integer(CN) ->
    case ?DICT:find(LN, M) of
        {ok, Row} ->
            ?DICT:find(CN, Row);
        error ->
            error
    end.

%%% @doc  Set value to matrix element.
%%% @spec (LN::integer(), CN::integer(), V, matrix()) -> matrix()
%%% @end
set(LN, CN, Value, M) when is_integer(LN), is_integer(CN) ->
    update(LN, CN, fun(_OldVal) -> Value end, Value, M).

%%% @doc  Update value of matrix element.
%%% @spec (LN::integer(), CN::integer(), F::((Val)->NewVal), Default, matrix()) -> matrix()
%%% @end
update(LN, CN, Updator, Default, M) when is_integer(LN), is_integer(CN) ->
    Row = case ?DICT:find(LN, M) of
        {ok, LRow} ->
            LRow;
        error ->
            ?DICT:new()
    end,
    NewRow = ?DICT:update(
        CN,
        Updator,
        Default,
        Row
    ),
    ?DICT:store(LN, NewRow, M).

%%% @doc  Retrieve a full row of the matrix.
%%% @spec (LN::integer(), matrix()) -> #mat_row{}
%%% @end
get_row(LN, M) ->
    case ?DICT:find(LN, M) of
        {ok, Row} ->
            #mat_row{line=LN, content=Row};
        error ->
            #mat_row{line=LN, content=?DICT:new()}
    end.

%%% @doc  Set a full row to the matrix.
%%% @spec (#mat_row{}, matrix()) -> #mat_row{}
%%% @end
set_row(Row, M) ->
    ?DICT:store(Row#mat_row.line, Row#mat_row.content, M).

%%% @doc  Get a value in a row of matrix.
%%% @spec (CN::integer(), #mat_row{}) -> {ok, Val}|error
%%% @end
get_row_value(CN, Row) ->
    ?DICT:find(CN, Row#mat_row.content).

%%% @doc  Set a value in a row of matrix.
%%% @spec (CN::integer(), Value, #mat_row{}) -> #mat_row{}
%%% @end
set_row_value(CN, Value, Row) ->
    Row#mat_row{content=?DICT:store(CN, Value, Row#mat_row.content)}.

%%% @doc  Update a value in a row of matrix.
%%% @spec (CN::integer(), F::((OldVal)->NewVal), DefaultVal, #mat_row{}) -> #mat_row{}
%%% @end
update_row_value(CN, Updator, Default, Row) ->
    Row#mat_row{content=?DICT:update(CN, Updator, Default, Row#mat_row.content)}.

%%% @doc  Fold a row of matrix.
%%% @spec ((CN::integer(), Value, Acc)->NewAcc, Acc, #mat_row{}) -> Acc
%%% @end
fold_row(Fun, Accumulator, Row) ->
    ?DICT:fold(Fun, Accumulator, Row#mat_row.content).

%%% @doc  Fold a matrix row by row.
%%% @spec ((#mat_row{}, Acc)->NewAcc, Acc, matrix()) -> Acc
%%% @end
fold_per_row(Fun, Accumulator, M) ->
    ?DICT:fold(
        fun(LN, RowData, Acc) ->
            % keep consistency with get_row format of one row
            Fun(#mat_row{line=LN, content=RowData}, Acc)
        end,
        Accumulator,
        M
    ).

%%% @doc  Map all row of a matrix.
%%% @spec ((#mat_row{})->R, matrix()) -> [{Line::integer(),R}]
%%% @end
map_per_row(Fun, M) ->
    ?DICT:to_list(?DICT:map(
        fun(LN, RowData) ->
            % keep consistency with get_row format of one row
            Fun(#mat_row{line=LN, content=RowData})
        end,
        M
    )).

% ~~ Implementation: Behaviour callbacks


% ~~ Implementation: Internal


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

newsetget_test_() -> [
    % this test also involve update (set use update)
    ?_assert(
        get(1,2,set(1,2,3,new())) == {ok, 3}
    ),
    ?_assert(
        get(1,3,set(1,2,3,new())) == error
    ),
    ?_assert(
        get(2,3,set(1,2,3,new())) == error
    ),
    ?_assert(
        get(2,2,set(1,2,3,new())) == error
    )
].

newsetgetrow_test_() -> [
    ?_assert(
        set_row(set_row_value(42,7,get_row(12,new())),new()) == set(12,42,7,new())
    )
].

foldmap_test_() -> [
    ?_assert(
        fold_row(
            fun(C,V,A) -> A+C*V end,
            0,
            get_row(1, set(1,1,10,set(1,2,100,new())))
        ) == 210
    ),
    ?_assert(
        fold_per_row(
            fun(R=#mat_row{line=L},A) -> {ok,V}=get_row_value(L,R), A+L*V end,
            0,
            set(2,2,100,set(1,1,10,new()))
        ) == 210
    ),
    ?_assert(
        lists:keysort(1, map_per_row(
            fun(R=#mat_row{line=L}) -> {ok,V}=get_row_value(L,R), L*V end,
            set(2,2,100,set(1,1,10,new()))
        )) == [{1,10}, {2,200}]
    )
].

-endif.
