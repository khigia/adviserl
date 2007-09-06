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
%%% @doc Main adviserl application and API.
%%%
%%% Refer to {@link adv_types} for type definitions.
%%% @end
-module(adviserl).

% ~~ Declaration: OTP
-behaviour(application).
-export([
    start/2,
    prep_stop/1,
    stop/1
]).


% ~~ Declaration: API

-export([
    % application management
    info/0,
    start_app/0,
    stop_node/1,
    % adviserl interface
    rate/3,
    rate_id/3,
    rate/4,
    rate_id/4,
    async_rate/3,
    async_rate_id/3,
    async_rate/4,
    async_rate_id/4,
    recommend_all/1,
    recommend_all/2
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%% @spec info() -> Proplist
%% @doc Return a list of information about adviserl application.
%%
%% This function doesn't fail even if application is not started.
%%
%% The key `is_started' give aviserl application status.
%% Information about services are group into keys `items',
%% `sources', `predictions' and `ratings'.
info() ->
    [
        {is_started, lists:keymember(
            adviserl,
            1,
            application:which_applications()
        )},
        {items, adv_items:info()},
        {sources, adv_sources:info()},
        {ratings, adv_ratings:info()},
        {predictions, adv_predictions:info()}
    ].

%%% @doc  Start localy the OTP application (run main application).
%%% @spec () -> ok
%%% @end
start_app() ->
    % run the main application
    ?DEBUG("Starting adviserl application", []),
    IsOK = application:start(adviserl),
    ?DEBUG("adviserl application status: ~p", [IsOK]),
    case (IsOK =:= ok) andalso adv_config:get_inets_start() of
        true ->
            IsInetsOK = application:start(inets),
            ?DEBUG("inets application status: ~p", [IsInetsOK]);
        _ ->
            ok
    end,
    IsOK.

%%% @doc  Stop the OTP application AND shutdown remote node AND local node!
%%% @spec ([Node]) -> ok
%%% @end
stop_node([Node]) ->
    case locate_node(Node) of
        {ok, FullNode} ->
            % stop the application,
            ?DEBUG("Stopping remote application", []),
            rpc:call(FullNode, application, stop, [adviserl]),
            ?DEBUG("Shutdown remote node", []),
            rpc:call(FullNode, init, stop, []),
            ?INFO("Stopped.", []);
        {error, Reason} ->
            ?WARNING("Can't locate application/node: ~s.", [Reason])
    end,
    init:stop().

%%% @doc  Add or change a rating from a SourceID about a ItemID.
%%% If SourceID or ItemID are not integer, ID are retrieve from
%%% adv_sources and adv_items, creating an entry if needed (SourceID
%%% and ItemID must always be some unique key, whatever type).
%%% @spec (sourceID(), itemID(), rating()) -> ok
%%% @todo API enforce call to both adv_ratings and adv_predictions:
%%% the code itself could implement some kind of automatic call back.
%%% Also, get,set and update imply 3 lookup for sourceID ratings!!!
%%% @end
rate(Source, Item, Rating={_RatingValue, _RatingData}) ->
    {ok, _IsSrcInserted, SourceID} = adv_sources:insert_new(Source, no_data),
    {ok, _IsItmInserted, ItemID} = adv_items:insert_new(Item, no_data),
    rate_id(SourceID, ItemID, Rating).

rate_id(SourceID, ItemID, Rating={_RatingValue, _RatingData}) ->
    % TODO: this OldRatings is not necessary!
    % adv_predictions should have a better API and not require it!
    OldRatings = adv_ratings:get_ratings(SourceID),
    adv_ratings:set_rating(SourceID, ItemID, Rating),
    adv_predictions:update_rating(
        SourceID,
        ItemID,
        Rating,
        OldRatings
    ).

async_rate(Source, Item, Rating={_RatingValue, _RatingData}) ->
    {ok, _IsSrcInserted, SourceID} = adv_sources:insert_new(Source, no_data),
    {ok, _IsItmInserted, ItemID} = adv_items:insert_new(Item, no_data),
    async_rate_id(SourceID, ItemID, Rating).

async_rate_id(SourceID, ItemID, Rating={_RatingValue, _RatingData}) ->
    adv_ratings:set_rating(SourceID, ItemID, Rating).

%%% @doc  Update a rating from a SourceID about a ItemID.
%%% If SourceID or ItemID are not integer, ID are retrieve from
%%% adv_sources and adv_items, creating an entry if needed (SourceID
%%% and ItemID must always be some unique key, whatever type).
%%% @spec (sourceID(), itemID(), (rating())->rating(), rating()) -> ok
%%% @todo API enforce call to both adv_ratings and adv_predictions:
%%% the code itself could implement some kind of automatic call back.
%%% Also, get,set and update imply 3 lookup for sourceID ratings!!!
%%% @end
rate(Source, Item, Updater, Default) ->
    {ok, _IsSrcInserted, SourceID} = adv_sources:insert_new(Source, no_data),
    {ok, _IsItmInserted, ItemID} = adv_items:insert_new(Item, no_data),
    rate_id(SourceID, ItemID, Updater, Default).

rate_id(SourceID, ItemID, Updater, Default) ->
    % this OldRatings is not necessary!
    % items should have a better API and not require it!
    OldRatings = adv_ratings:get_ratings(SourceID),
    adv_ratings:update_rating(SourceID, ItemID, Updater, Default),
    Rating = adv_ratings:get_rating(SourceID, ItemID),
    adv_predictions:update_rating(
        SourceID,
        ItemID,
        Rating,
        OldRatings
    ).
        
async_rate(Source, Item, Updater, Default) ->
    {ok, _IsSrcInserted, SourceID} = adv_sources:insert_new(Source, no_data),
    {ok, _IsItmInserted, ItemID} = adv_items:insert_new(Item, no_data),
    async_rate_id(SourceID, ItemID, Updater, Default).

async_rate_id(SourceID, ItemID, Updater, Default) ->
    adv_ratings:update_rating(SourceID, ItemID, Updater, Default).


%% @spec recommend_all(IDOrKeyOrRatings) -> predictions() | {error,Reason}
%% @doc  Retrieve prediction for each item, using default options.
%% Call recommend_all/2 with default value for options (sorted and strict);
%% See documentation of recommend_all/2 for interpretation of parameter.
%% @see recommend_all/2
recommend_all(IDOrKeyOrRatings) ->
    recommend_all(IDOrKeyOrRatings, []).

%% @spec recommend_all(Source, [Option]) -> predictions() | {error, Reason}
%%     Source = sourceID() | [{itemID(), ratingValue()}] | term()
%%     Option = {no_sorted, bool()}
%%            | {no_strict, bool()}
%%            | {no_remove_known, bool()}
%%            | {no_key_lookup, bool()}
%% @doc  Retrieve prediction for each itemID.
%%
%% Interpretation of Source depends on its type:
%% <ul>
%%     <li>if integer, Source is seen as ID;</li>
%%     <li>if empty list or list beginning by one tuple, Source is seen as ratings;</li>
%%     <li>else Source is seen as key.</li>
%% </ul>
%%
%% Defaults options are
%% <ul>
%%     <li>`{no_sorted,false}': predictions are sorted by decreasing values; if true the sort may not be done.</li>
%%     <li>`{no_strict,false}': return only strictly positive predictions; if true predictions may contain some item with null prediction score.</li>
%%     <li>`{no_remove_known,false}': predictions contains only not rated items; if true predictions may contain already rated items.</li>
%%     <li>`{no_key_lookup,false}': predictions is a list of item's keys; if true predictions is a list of item's internal ID (avoid one lookup per prediction).</li>
%% </ul>
recommend_all(SourceID, Options)
    when
        is_integer(SourceID),
        is_list(Options)
    ->
    recommend_all_id(SourceID, Options);

recommend_all(RatingValues=[], Options)
    when
        is_list(Options)
    ->
    recommend_all_ratings(RatingValues, Options);

recommend_all(RatingValues=[Rating|_], Options)
    when
        is_tuple(Rating),
        is_list(Options)
    ->
    recommend_all_ratings(RatingValues, Options);

recommend_all(SourceKey, Options)
    when
        is_list(Options)
    ->
    recommend_all_key(SourceKey, Options).


% ~~ Implementation: Behaviour callbacks

%%% @doc  Start adviserl OTP application and Mnesia if not running.
%%% @see  adv_adviserl_sup:start_link/0
%%% @end
start(_StartType, _StartArgs) ->
    % run the main supervisor
    adv_adviserl_sup:start_link().

prep_stop(State) ->
    %save_files(),
    State.

%%% @doc  Stop adviserl OTP application and Mnesia if started by init.
%%% @spec (State) -> ok | {error, Error}
%%% @end
stop(State) ->
    ?DEBUG("adviserl stopped.", []),
    State.


% ~~ Implementation: Internal

locate_node(MaybeLocalNode) ->
    case net_adm:ping(MaybeLocalNode) of
        pong ->
            {ok, MaybeLocalNode};
        _ ->
            {ok, LocalHost} = inet:gethostname(),
            LNodeStr = atom_to_list(MaybeLocalNode) ++ "@" ++ LocalHost,
            LNodeAtom = list_to_atom(LNodeStr),
            case net_adm:ping(LNodeAtom) of
                pong ->
                    {ok, LNodeAtom};
                _ ->
                    {
                        error,
                        "Can't find node " ++ LNodeStr ++
                            " on host " ++ net_adm:localhost()
                    }
            end
    end.

recommend_all_key(SourceKey, Options) ->
    case adv_sources:id_from_key(SourceKey) of
        SourceID when is_integer(SourceID) ->
            recommend_all_id(SourceID, Options);
        _ ->
            {error, unknown_source_key}
    end.

recommend_all_id(SourceID, Options) ->
    case adv_ratings:get_ratings(SourceID) of
        undefined ->
            {error, unknown_source_id};
        Ratings ->
            predict_all(Ratings, Options)
    end.

recommend_all_ratings(RatingValues, Options) ->
    Ratings = adv_ratings:from_list(RatingValues),
    predict_all(Ratings, Options).

predict_all(Ratings, Options) ->
    Predictions0 = adv_predictions:predict_all(Ratings, Options),
    Predictions1 = case proplists:get_bool(no_key_lookup, Options) of
        false ->
            %TODO those lookups could be done in one message to adv_items...
            lists:reverse(lists:foldl(
                fun({ItemID, Score}, Acc) ->
                    case adv_items:key_from_id(ItemID) of
                        {ok, Key} ->
                            [{Key, Score}|Acc];
                        _ ->
                            Acc
                    end
                end,
                [],
                Predictions0
            ));
        _ ->
            Predictions0
    end,
    Predictions1.


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
