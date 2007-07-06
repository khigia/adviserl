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
%%% @doc Extensions to the array API.
%%%
%%% @end
-module(adv_array_ext).

% ~~ Declaration: API

-export([
    update/3
]).


% ~~ Declaration: Internal
%empty


% ~~ Implementation: API

%%% @doc  Update the value of an array at a given index through a function.
%%% @spec (Index::integer(), UpdatorFun::(OldValue::term()) -> term(), Array) -> NewArray
%%% @end
update(Index, Updator, Array)
    when
        is_integer(Index),
        is_function(Updator, 1)
    ->
    PreviousValue = case (catch array:get(Index, Array)) of
        {'EXIT', {badarg, _}} ->
            array:default(Array);
        Value ->
            Value
    end,
    array:set(Index, Updator(PreviousValue), Array).


% ~~ Implementation: Internal
%empty


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

update_test_() -> [
    ?_assert( % update existing element
        array:to_list(update(1,fun(X)->2*X end,array:set(1,12,array:new(0, 42)))) == [42, 24]
    ),
    ?_assert( % update unknow element
        array:to_list(update(2,fun(X)->2*X end,array:set(1,12,array:new(0, 42)))) == [42, 12, 84]
    )
].

-endif.
