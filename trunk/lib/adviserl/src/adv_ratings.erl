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
%%% @copyright 2007 Affle Pvt. Ltd.
%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @doc Interface of storage management of ratings per source.
%%%
%%% This module provide convenience functions to call the ratings gen_server.
%%% Furthermore it provides functions to manipulate some data types (but those
%%% function may be avoided for performance reason because they have to
%%% lookup each time for the gen_server module name).
%%%
%%% Refer to {@link adv_types} for type definitions.
%%% @end
-module(adv_ratings).


% ~~ Declaration: API
-export([
    % ratings interface
    load_file/2,
    save_file/2,
    get_ratings/1,
    get_rating/2,
    set_rating/3,
    update_rating/4,
    fold_sources/2,
    % convenience functions
    from_list/1,
    to_list/1,
    fold_ratings/3
]).
-export([
    print_debug/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

load_file(File, Options) ->
    R = gen_server:call(
        ?RATINGS_PNAME,
        {load_file, File, Options},
        infinity
    ),
    ?INFO("file load status: ~w", [R]),
    R.

save_file(File, Options) ->
    R = gen_server:call(
        ?RATINGS_PNAME,
        {save_file, File, Options},
        infinity
    ),
    ?INFO("file save status: ~w", [R]),
    R.

%%% @spec get_ratings(sourceID()) -> {ok, ratings()}|undefined
%%%
%%% @doc  Get all ratings of a source.
%%% @todo remove infinity timeout!
%%% @end
get_ratings(SourceID) when is_integer(SourceID) ->
    gen_server:call(
        ?RATINGS_PNAME,
        {get_ratings, SourceID},
        infinity
    ).

%%% @spec get_rating(sourceID(), itemID()) -> rating()|undefined
%%%
%%% @doc  Get rating of a source for one item.
%%% @todo remove infinity timeout!
%%% @end
get_rating(SourceID, ItemID)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0
    ->
    gen_server:call(
        ?RATINGS_PNAME,
        {get_rating, SourceID, ItemID},
        infinity
    ).

%%% @spec set_rating(sourceID(), itemID(), rating()) -> ok
%%%
%%% @doc  Add/change one rating.
%%% @todo call->cast
%%% @todo remove infinity timeout!
%%% @end
set_rating(SourceID, ItemID, Rating)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0
    ->
    gen_server:call(
        ?RATINGS_PNAME,
        {set_rating, SourceID, ItemID, Rating},
        infinity
    ).

%%% @spec update_rating(sourceID(), itemID(), (rating())->rating(), rating()) -> ok
%%%
%%% @doc  Update one rating.
%%% @todo call->cast
%%% @end
update_rating(SourceID, ItemID, Updater, Default)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0,
        is_function(Updater, 1)
    ->
    gen_server:call(
        ?RATINGS_PNAME,
        {update_rating, SourceID, ItemID, Updater, Default},
        infinity
    ).

%%% @spec fold_sources(((sourceID(),ratings(),Acc)->Acc), Acc) -> Acc
%%%
%%% @doc  Fold all sources set-of-ratings by set-of-ratings.
%%% @todo remove infinity timeout!
%%% @end
fold_sources(Fun, Accumulator) ->
    gen_server:call(
        ?RATINGS_PNAME,
        {fold_sources, Fun, Accumulator},
        infinity
    ).

%%% @spec from_list([{itemID(),ratingValue()}]) -> ratings()
%%%
%%% @doc  Create a ratings structure from a list of key-value pairs.
%%% @todo Check for errors
%%% @end
from_list(RatingKeyValues) when is_list(RatingKeyValues) ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:from_list(RatingKeyValues).

%%% @spec to_list(ratings()) -> [{itemID(),ratingValue()}]
%%%
%%% @doc  Create a list of key-value pairs from a ratings structure.
%%% @todo Check for errors
%%% @end
to_list(Ratings) ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:to_list(Ratings).

%%% @spec fold_ratings(((itemID(),rating(),Acc)->Acc), Acc, ratings()) -> Acc
%%%
%%% @doc  Fold a ratings structure passing rating by rating to the function.
%%% @end
fold_ratings(Fun, Accumulator, SourceRatings) ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:fold_ratings(Fun, Accumulator, SourceRatings).

%%% @doc  Print the full matrix in debug mode.
%%% @spec () -> ok
%%% @end
print_debug() ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:print_debug().



% ~~ Implementation: Internal
%empty


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
