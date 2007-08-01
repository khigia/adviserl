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
%%% @doc Interface to predictions management.
%%%
%%% @end
-module(adv_predictions).


% ~~ Declaration: API
-export([
    init/0,
    update_rating/4,
    predict_all/2
]).
-export([
    print_debug/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%%% @doc  Init the server by reloading the whole ratings collection.
%%% @spec () -> ok
%%% @todo remove infinity timeout
%%% @end
init() ->
    gen_server:call(
        ?PREDICTIONS_PNAME,
        init,
        infinity
    ).

%%% @doc  Update rating influence on the recommender algorithm.
%%% @spec (sourceID(), itemID(), NewRating::rating(), OldRatings::ratings()|undefined) -> ok
%%% @todo remove infinity timeout
%%% @end
update_rating(SourceID, ItemID, NewRating, OldRatings)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0
    ->
    gen_server:call(
        ?PREDICTIONS_PNAME,
        {update_rating, SourceID, ItemID, NewRating, OldRatings},
        infinity
    ).

%%% @doc  For a given set of ratings, give prediction for all items.
%%% @see  adviserl:recommend_all/2
%%% @spec (ratings(), [Option]) -> predictions()
%%% @todo remove infinity timeout
%%% @end
predict_all(Ratings, Options) ->
    gen_server:call(
        ?PREDICTIONS_PNAME,
        {predict_all, Ratings, Options},
        infinity
    ).

%%% @doc  Print the full matrix in debug mode.
%%% @spec () -> ok
%%% @todo remove infinity timeout
%%% @end
print_debug() ->
    gen_server:call(
        ?PREDICTIONS_PNAME,
        print_debug,
        infinity
    ).


% ~~ Implementation: Internal
%empty


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
