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
    stop/1
]).


% ~~ Declaration: API

-export([
    start_app/0,
    stop_node/1,
    rate/3,
    rate/4,
    recommend_all/1,
    recommend_all/2
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%%% @doc  Start localy the OTP application (run main application).
%%% @spec () -> ok
%%% @end
start_app() ->
    % run the main application
    ?DEBUG("Starting the main OTP application", []),
    IsOK = application:start(adviserl),
    ?DEBUG("adviserl application status: ~p", [IsOK]).

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
rate(SourceID, ItemID, Rating={_RatingValue, _RatingData})
    when
        is_integer(SourceID),
        is_integer(ItemID)
    ->
    % TODO: this OldRatings is not necessary!
    % adv_predictions should have a better API and not require it!
    OldRatings = adv_ratings:get_ratings(SourceID),
    adv_ratings:set_rating(SourceID, ItemID, Rating),
    adv_predictions:update_rating(
        SourceID,
        ItemID,
        Rating,
        OldRatings
    );
rate(Source, Item, Rating) when not is_integer(Source) ->
    {_IsInserted, ID} = adv_sources:insert_new(Source, no_data),
    rate(ID, Item, Rating);
rate(Source, Item, Rating) when not is_integer(Item) ->
    {_IsInserted, ID} = adv_items:insert_new(Item, no_data),
    rate(Source, ID, Rating).

%%% @doc  Update a rating from a SourceID about a ItemID.
%%% If SourceID or ItemID are not integer, ID are retrieve from
%%% adv_sources and adv_items, creating an entry if needed (SourceID
%%% and ItemID must always be some unique key, whatever type).
%%% @spec (sourceID(), itemID(), (rating())->rating(), rating()) -> ok
%%% @todo API enforce call to both adv_ratings and adv_predictions:
%%% the code itself could implement some kind of automatic call back.
%%% Also, get,set and update imply 3 lookup for sourceID ratings!!!
%%% @end
rate(SourceID, ItemID, Updater, Default)
    when
        is_integer(SourceID),
        is_integer(ItemID)
    ->
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
    );
rate(Source, Item, Updater, Default) when not is_integer(Source) ->
    {_IsInserted, ID} = adv_sources:insert_new(Source, no_data),
    rate(ID, Item, Updater, Default);
rate(Source, Item, Updater, Default) when not is_integer(Item) ->
    {_IsInserted, ID} = adv_items:insert_new(Item, no_data),
    rate(Source, ID, Updater, Default).
        
%%% @doc  Retrieve prediction for each itemID with default options.
%%% Call recommend_all/2 with default value for options (sorted and strict).
%%% @see recommend_all/2
recommend_all(IDOrRatings) ->
    recommend_all(IDOrRatings, []).

%%% @doc  Retrieve prediction for each itemID.
%%%
%%% If option sorted is true, sort predictions by decreasing values.
%%%
%%% If option strict is true, return only strictly positive predictions.
%%% @spec (Source::sourceID()|[{itemID(), ratingValue()}], [Option]) -> predictions()|{error, Reason}
%%%     Option = {no_sorted, bool()} | {no_strict, bool()} | {no_remove_known, bool()}
%%% @end
recommend_all(SourceID, Options) when is_integer(SourceID), is_list(Options) ->
    case adv_ratings:get_ratings(SourceID) of
        undefined ->
            {error, "SourceID not recognized"};
        Ratings ->
            adv_predictions:predict_all(Ratings, Options)
    end;
recommend_all(RatingValues, Options) when is_list(RatingValues), is_list(Options) ->
    Ratings = adv_ratings:from_list(RatingValues),
    adv_predictions:predict_all(Ratings, Options).


% ~~ Implementation: Behaviour callbacks

%%% @doc  Start the OTP application (run main supervisor).
%%% @see  adv_adviserl_sup:start_link/0
%%% @end
start(_StartType, _StartArgs) ->
    %TODO later we may want to pass the recommender callback module as argument
    % run the main supervisor
    adv_adviserl_sup:start_link().

%%% @doc  Stop the OTP application.
%%% @spec (State) -> State
%%% @end
stop(State) ->
    % close the main supervisor
    State.


% ~~ Implementation: Internal

locate_node(MaybeLocalNode) ->
    case net_adm:ping(MaybeLocalNode) of
        pong ->
            {ok, MaybeLocalNode};
        _ ->
            LNodeStr = atom_to_list(MaybeLocalNode) ++ "@localhost",
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



% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
