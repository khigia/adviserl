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
%%% @doc Storage management of ratings per source.
%%%
%%% Refer to {@link adv_types} for type definitions.
%%%
%%% @end
-module(adv_ratings).


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
-export([
    start_link/0,
    get_rating/2,
    set_rating/3,
    update_rating/4,
    get_ratings/1,
    from_list/1,
    to_list/1,
    fold_sources/2,
    fold_ratings/3
]).
-export([
    print_debug/0
]).


% ~~ Declaration: Internal

-record(state, {
    ratings
}).

-include("include/adviserl.hrl").

-define(DICT, dict).



% ~~ Implementation: API

%%% @doc  Start server localy registered.
%%% @see  gen_server:start_link/4
%%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%% @doc  Add/change one rating.
%%% @spec (sourceID(), itemID(), rating()) -> ok
%%% @todo call->cast
%%% @end
set_rating(SourceID, ItemID, Rating)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0
    ->
    gen_server:call(?MODULE, {set_rating, SourceID, ItemID, Rating}, infinity).

%%% @doc  Get rating of a source for one item.
%%% @spec (sourceID(), itemID()) -> rating()|undefined
%%% @todo remove infinity timeout!
%%% @end
get_rating(SourceID, ItemID)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0
    ->
    gen_server:call(?MODULE, {get_rating, SourceID, ItemID}, infinity).

%%% @doc  Update one rating.
%%% @spec (sourceID(), itemID(), (rating())->rating(), rating()) -> ok
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
    gen_server:call(?MODULE, {update_rating, SourceID, ItemID, Updater, Default}, infinity).

%%% @doc  Get all ratings of a source.
%%% @spec (sourceID()) -> {ok, ratings()}|undefined
%%% @todo remove infinity timeout!
%%% @end
get_ratings(SourceID) when is_integer(SourceID) ->
    gen_server:call(?MODULE, {get_ratings, SourceID}, infinity).

%%% @doc  Fold all sources set-of-ratings by set-of-ratings.
%%% @spec (((sourceID(),ratings(),Acc)->Acc), Acc) -> Acc
%%% @todo remove infinity timeout!
%%% @end
fold_sources(Fun, Accumulator) ->
    gen_server:call(?MODULE, {fold_sources, Fun, Accumulator}, infinity).

%%% @doc  Create a ratings structure from a list of key-value pairs.
%%% @spec ([{itemID(),ratingValue()}]) -> ratings()
%%% @todo Check for errors
%%% @end
from_list(RatingKeyValues) when is_list(RatingKeyValues) ->
    Ratings = lists:map(
        fun({Key,Val}) -> {Key, {Val, nodata}} end,
        RatingKeyValues
    ),
    ?DICT:from_list(Ratings).

%%% @doc  Create a list of key-value pairs from a ratings structure.
%%% @spec (ratings()) -> [{itemID(),ratingValue()}]
%%% @todo Check for errors
%%% @end
to_list(Ratings) ->
    lists:map(
        fun({Key, {RatingValue, _RatingData}}) -> {Key, RatingValue} end,
        ?DICT:to_list(Ratings)
    ).

%%% @doc  Fold a ratings structure passing rating by rating to the function.
%%% @spec (((itemID(),rating(),Acc)->Acc), Acc, ratings()) -> Acc
%%% @end
fold_ratings(Fun, Accumulator, SourceRatings) ->
    fold_ratings_priv(Fun, Accumulator, SourceRatings).

%%% @doc  Print the full matrix in debug mode.
%%% @spec () -> ok
%%% @end
print_debug() ->
    RatingsToList = fun(Ratings) ->
        List0 = fold_ratings(
            fun(ItemID, {Val,_Dta}, Acc) ->
                Acc ++ [{ItemID,Val}]
            end,
            [],
            Ratings
        ),
        List1 = lists:keysort(2, List0),
        List1
    end,
    Printer = fun(SourceID, Ratings, Acc) ->
        io:format("src ~w: ~w", [SourceID, RatingsToList(Ratings)]),
        Acc
    end,
    fold_sources(Printer, ok).


% ~~ Implementation: Behaviour callbacks

init(_InitArgs) ->
    State = #state{
        ratings = init_ratings()
    },
    {ok, State}.

handle_call({set_rating, SourceID, ItemID, Rating}, _From, State) ->
    AllRatings = set_rating(SourceID, ItemID, Rating, State#state.ratings),
    {reply, ok, State#state{ratings = AllRatings}};

handle_call({get_rating, SourceID, ItemID}, _From, State) ->
    R = get_rating(SourceID, ItemID, State#state.ratings),
    {reply, R, State};

handle_call({update_rating, SourceID, ItemID, Updater, Default}, _From, State) ->
    AllRatings = update_rating(
        SourceID, ItemID, Updater, Default, State#state.ratings
    ),
    {reply, ok, State#state{ratings = AllRatings}};

handle_call({get_ratings, SourceID}, _From, State) ->
    RR = get_ratings(SourceID, State#state.ratings),
    {reply, RR, State};

handle_call({fold_sources, Fun, Acc}, From, State) ->
    %R = fold_sources(Fun, Acc, State#state.ratings),
    %{reply, R, State};
    spawn(fun() ->
        R = fold_sources(Fun, Acc, State#state.ratings),
        gen_server:reply(From, R)
    end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

%%% @doc  Create a data structure to contain all ratings of all sources.
%%% @spec () -> AllRatings
%%% @private
%%% @todo use ets/mnesia and add a file read/write
%%% @end
init_ratings() ->
    ?DICT:new().

%%% @doc  Add or modify one rating.
%%% @spec (sourceID(), itemID(), rating(), AllRatings) -> AllRatings
%%% @private
%%% @end
set_rating(SourceID, ItemID, Rating, Ratings) ->
    update_rating(
        SourceID,
        ItemID,
        fun(_Previous) -> Rating end,
        Rating,
        Ratings
    ).

%%% @doc  Retrieve one rating for one source-item pair.
%%% @spec (sourceID(), itemID(), AllRatings) -> rating()|undefined
%%% @private
%%% @end
get_rating(SourceID, ItemID, Ratings) ->
    case ?DICT:find(SourceID, Ratings) of
        {ok, Row} ->
            case ?DICT:find(ItemID, Row) of
                {ok, Rating} ->
                    Rating;
                error ->
                    undefined
            end;
        error ->
            undefined
    end.

%%% @doc  Update a rating.
%%% @spec (sourceID(), itemID(), (rating())->rating(), rating(), AllRatings) -> AllRatings
%%% @private
%%% @end
update_rating(SourceID, ItemID, Updater, Default, Ratings) ->
    ?DICT:update(
        SourceID,
        fun(Row) ->
            ?DICT:update(
                ItemID,
                Updater,
                Default,
                Row
            )
        end,
        ?DICT:store(ItemID, Default, ?DICT:new()),
        Ratings
    ).

%%% @doc  Retrieve all ratings for one source.
%%% @spec (sourceID(), AllRatings) -> {ok, ratings()}|undefined
%%% @private
%%% @end
get_ratings(SourceID, Ratings) ->
    case ?DICT:find(SourceID, Ratings) of
        {ok, Row} ->
            Row;
        error ->
            undefined
    end.

%%% @doc  Fold for each set of ratings.
%%% @spec (Fun::(sourceID(), ratings(), Acc)->Acc, Acc, AllRatings)->Acc
%%% @private
%%% @end
fold_sources(Fun, Acc, Ratings) ->
    ?DICT:fold(Fun, Acc, Ratings).

%%% @doc  Fold for each rating of one source.
%%% @spec (Fun::(itemID(), rating(), Acc)->Acc, Acc, ratings())->Acc
%%% @private
%%% @end
fold_ratings_priv(Fun, Accumulator, SourceRatings) ->
    ?DICT:fold(Fun, Accumulator, SourceRatings).


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

%%% @hidden
setget_rating_test_() -> [
    ?_assert(
        get_rating(1,2,set_rating(1,2,{3,some_data},init_ratings())) == {3, some_data}
    ),
    ?_assert(
        get_rating(1,4,set_rating(1,2,{3,some_data},init_ratings())) == undefined
    ),
    ?_assert(
        get_rating(4,2,set_rating(1,2,{3,some_data},init_ratings())) == undefined
    ),
    ?_assert(
        get_rating(1,2,set_rating(1,2,{3,some_data},init_ratings())) /= {4, some_data}
    ),
    ?_assert(
        get_rating(1,2,set_rating(1,2,{3,some_data},init_ratings())) /= {3, other_data}
    )
].

%%% @hidden
pub_setget_rating_test() ->
    start_link(),
    set_rating(1,2,{3,some_data}),
    ?assert(get_rating(1,2) == {3, some_data}),
    ?assert(get_rating(1,4) == undefined),
    ?assert(get_rating(4,2) == undefined).

-endif.
