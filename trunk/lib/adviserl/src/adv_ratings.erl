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
%% @copyright 2007 Affle Pvt. Ltd.
%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%% @doc Interface of storage management of ratings per source.
%%
%% This module provide convenience functions to call the ratings gen_server.
%%
%% Furthermore it provides functions to manipulate some data types (but those
%% function may be avoided for performance reason because they have to
%% lookup each time for the gen_server module name).
%%
%% == Implementation of a ratings' service module ==
%%
%% A ratings' module must implement a `gen_server' behaviour.
%%
%% Server must respond to the following calls:
%% <ul>
%%     <li></li>
%% </ul>
%%
%% Server must also respond to the following casts:
%% <ul>
%%     <li></li>
%% </ul>
%%
%% Furthermore the module must also implement the following functions:
%% <ul>
%%     <li>`from_list'</li>
%%     <li>`to_list'</li>
%%     <li>`fold_ratings'</li>
%%     <li>`print_debug'</li>
%% </ul>
%%
%% @end
-module(adv_ratings).


% ~~ Declaration: API
-export([
    info/0,
    % ratings interface
    get_ratings/1,
    get_rating/2,
    set_rating/3,
    update_rating/4,
    fold_sources/2,
    % auxiliary functions
    load_file/2,
    save_file/2,
    % convenience functions
    from_list/1,
    to_list/1,
    fold_ratings/3
]).
-export([
    % debug only
    print_debug/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%% @spec () -> Proplist::list()
%% @doc Return informations on adviserl rating service configuration and state.
%%
%% This function never fails (service started or not).
%%
%% The return value is a property list with at least two keys:
%% <code>process_name</code> and <code>module_spec</code>;
%% other keys may be added by the running service implementation.
%%
%% The result of info should not be dependent of execution's context: this way
%% it can be use as a backup consistency check, meaning that storing info while
%% writing backup and checking info matching before restoring enable to
%% check adviserl configuration compatibility (service implementation etc).
info() ->
    CommonInfo = [
        {process_name, ?RATINGS_PNAME},
        {module_spec, adv_config:get_ratings_behaviour()}
        % following is too much dependent of context
        % {whereis, erlang:whereis(?RATINGS_PNAME)}
    ],
    MaybeInfo = (catch gen_server:call(
        ?RATINGS_PNAME,
        info
    )),
    case MaybeInfo of
        {'EXIT', _} ->
            CommonInfo;
        Info ->
            CommonInfo ++ Info
    end.


%% @spec (adv_types:sourceID()) -> {ok, adv_types:ratings()} | undefined | {error,Reason}
%%
%% @doc  Get all ratings of a source.
%% @todo remove infinity timeout!
%% @end
get_ratings(SourceID) when is_integer(SourceID) ->
    gen_server:call(
        ?RATINGS_PNAME,
        {get_ratings, SourceID},
        infinity
    ).

%% @spec (adv_types:sourceID(), adv_types:itemID()) -> adv_types:rating() | undefined | {error,Reason}
%%
%% @doc  Get rating of a source for one item.
%% @todo remove infinity timeout!
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

%% @spec set_rating(sourceID(), itemID(), rating()) -> ok | {error,Reason}
%%
%% @doc  Add/change one rating.
%% @todo call->cast
%% @todo remove infinity timeout!
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

%% @spec update_rating(sourceID(), itemID(), (rating())->rating(), rating()) -> ok | {error,Reason}
%%
%% @doc  Update one rating.
%% @todo call->cast
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

%% @spec fold_sources(Fun::function(), term()) -> term() | {error,Reason}
%%     Fun = ((sourceID(), ratings(), Acc) -> Acc)
%%
%% @doc  Fold all sources giving all ratings to the function for each source.
%% @todo remove infinity timeout!
fold_sources(Fun, Accumulator) ->
    gen_server:call(
        ?RATINGS_PNAME,
        {fold_sources, Fun, Accumulator},
        infinity
    ).


%% @spec (string(), list()) -> ok | {error, Reason}
%%
%% @doc Load service's data from disc.
%%
%% `File' parameter is a recommendation (may not be used, e.g. mnesia).
%% `Options' interpretation also depend of service implementation.
load_file(File, Options) ->
    R = gen_server:call(
        ?RATINGS_PNAME,
        {load_file, File, Options},
        infinity
    ),
    ?INFO("file load status: ~w", [R]),
    R.

%% @spec (string(), list()) -> ok | {error, Reason}
%%
%% @doc Save service's data on disc.
%%
%% `File' parameter is a recommendation (may not be used, e.g. mnesia).
%% `Options' interpretation also depend of service implementation.
save_file(File, Options) ->
    R = gen_server:call(
        ?RATINGS_PNAME,
        {save_file, File, Options},
        infinity
    ),
    ?INFO("file save status: ~w", [R]),
    R.


%% @spec from_list([{itemID(),ratingValue()}]) -> ratings()
%%
%% @doc  Create a ratings structure from a list of key-value pairs.
from_list(RatingKeyValues) when is_list(RatingKeyValues) ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:from_list(RatingKeyValues).

%% @spec to_list(ratings()) -> [{itemID(),ratingValue()}]
%%
%% @doc  Create a list of key-value pairs from a ratings structure.
to_list(Ratings) ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:to_list(Ratings).

%% @spec fold_ratings(((itemID(),rating(),Acc)->Acc), Acc, ratings()) -> Acc
%%
%% @doc  Fold a ratings structure passing rating by rating to the function.
fold_ratings(Fun, Accumulator, SourceRatings) ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:fold_ratings(Fun, Accumulator, SourceRatings).


%% @spec () -> ok
%%
%% @doc Print state information (may cause huge output).
print_debug() ->
    RatingMod = adv_config:get_ratings_module(),
    RatingMod:print_debug().



% ~~ Implementation: Internal
%empty


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
% few tests are possible as this module is only an interface

%% @hidden
info_test() ->
    ?assert(proplists:get_value(process_name, info()) == ?RATINGS_PNAME).

-endif.
