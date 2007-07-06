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
%%% @doc 2D SkewMatrix.
%%%
%%% 2D skew-symmetric (antisymmetric) matrix API (wrapper on matrix module).
%%% AntiSymmetric means here: A_i,j = -A_j,i and A_i,i = 0
%%%
%%% Only lower left part of matrix is stored: columnNumber &lt; lineNumber
%%%
%%% @end
-module(adv_mat_sm).


% ~~ Declaration: OTP relative


% ~~ Declaration: API
-export([
    new/0,
    new/1,
    get/3,
    set/4,
    update/5,
    get_partial_row/2,
    set_partial_row/2,
    get_partial_row_value/2,
    set_partial_row_value/3,
    update_partial_row_value/4,
    fold_per_partial_row/3,
    fold_partial_row/3,
    map_per_partial_row/2
]).


% ~~ Declaration: Internal
-define(MAT, adv_mat_dm).% TODO could be module parameter

-include("include/adv_mat.hrl").

-record(sm, {
    f, % function to compute anti symmetry
    m  % backend matrix state
}).


% ~~ Implementation: API

new() ->
    new(fun(X) -> -X end).
    
new(ASFun) ->
    #sm{f=ASFun, m=?MAT:new()}.

get(_N, _N, _M) ->
    error;
get(LN, CN, M) when CN < LN ->
    ?MAT:get(LN, CN, M#sm.m);
get(LN, CN, M) when CN > LN ->
    case ?MAT:get(CN, LN, M#sm.m) of
        {ok, Val} ->
            {ok, (M#sm.f)(Val)};
        error ->
            error
    end.

set(LN, CN, Value, M) ->
    M#sm{m=update(LN, CN, fun(_OldVal) -> Value end, Value, M#sm.m)}.

update(N, N, _Updator, _Default, _M) ->
    throw({error, "?MODULE implement antisymmetric matrix: A[i][i]=0"});
update(LN, CN, Updator, Default, M) when CN < LN ->
    M#sm{m=?MAT:update(LN, CN, Updator, Default, M#sm.m)};
update(LN, CN, Updator, Default, M) when CN > LN ->
    ASFun = M#sm.f,
    M#sm{m=?MAT:update(
        CN,
        LN,
        fun(Val) -> ASFun(Updator(ASFun(Val))) end,
        Default,
        M#sm.m
    )}.

get_partial_row(LN, M) ->
    ?MAT:get_row(LN, M#sm.m).

set_partial_row(Row, M) ->
    M#sm{m=?MAT:set_row(Row, M#sm.m)}.

get_partial_row_value(CN, Row=#mat_row{line=LN}) when CN < LN ->
    ?MAT:get_row_value(CN, Row);
get_partial_row_value(CN, #mat_row{line=LN}) when CN >= LN ->
    throw({error, "?MODULE implement antisymmetric matrix: can't get A[i][j] for j>=i in row"}).

set_partial_row_value(CN, Value, Row=#mat_row{line=LN}) when CN < LN ->
    ?MAT:set_row_value(CN, Value, Row);
set_partial_row_value(CN, _Value, #mat_row{line=LN}) when CN >= LN ->
    throw({error, "?MODULE implement antisymmetric matrix: can't set A[i][j] for j>=i in row"}).

update_partial_row_value(CN, Updator, Default, Row=#mat_row{line=LN}) when CN < LN ->
    ?MAT:update_row_value(CN, Updator, Default, Row);
update_partial_row_value(CN, _Updator, _Default, #mat_row{line=LN}) when CN >= LN ->
    throw({error, "?MODULE implement antisymmetric matrix: can't update A[i][j] for j>=i in row"}).

fold_partial_row(Fun, Accumulator, PartialRow) ->
    ?MAT:fold_row(Fun, Accumulator, PartialRow).

fold_per_partial_row(Fun, Accumulator, M) ->
    ?MAT:fold_per_row(Fun, Accumulator, M#sm.m).

map_per_partial_row(Fun, M) ->
    ?MAT:map_per_row(Fun, M#sm.m).

% ~~ Implementation: Behaviour callbacks


% ~~ Implementation: Internal


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
