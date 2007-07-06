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
    rate/3,
    rate/4,
    recommend_all/1,
    recommend_all/2
]).

%%% @doc  Add or change a rating from a SourceID about a ItemID.
%%% @spec (sourceID(), itemID(), rating()) -> ok
%%% @todo API enforce call to both adv_ratings and adv_items:
%%% the code itself could implement some kind of automatic call back.
%%% Also, get,set and update imply 3 lookup for sourceID ratings!!!
%%% @end
rate(SourceID, ItemID, Rating={_RatingValue, _RatingData}) ->
    % this OldRatings is not necessary!
    % items should have a better API and not require it!
    OldRatings = adv_ratings:get_ratings(SourceID),
    adv_ratings:set_rating(SourceID, ItemID, Rating),
    adv_items:update_rating(
        SourceID,
        ItemID,
        Rating,
        OldRatings
    ).

%%% @doc  Update a rating from a SourceID about a ItemID.
%%% @spec (sourceID(), itemID(), (rating())->rating(), rating()) -> ok
%%% @todo API enforce call to both adv_ratings and adv_items:
%%% the code itself could implement some kind of automatic call back.
%%% Also, get,set and update imply 3 lookup for sourceID ratings!!!
%%% @end
rate(SourceID, ItemID, Updater, Default) ->
    % this OldRatings is not necessary!
    % items should have a better API and not require it!
    OldRatings = adv_ratings:get_ratings(SourceID),
    adv_ratings:update_rating(SourceID, ItemID, Updater, Default),
    Rating = adv_ratings:get_rating(SourceID, ItemID),
    adv_items:update_rating(
        SourceID,
        ItemID,
        Rating,
        OldRatings
    ).
        
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
%%%     Option = {no_sorted, bool()} | {no_strict, bool()}
%%% @end
recommend_all(SourceID, Options) when is_integer(SourceID), is_list(Options) ->
    case adv_ratings:get_ratings(SourceID) of
        undefined ->
            {error, "SourceID not recognized"};
        Ratings ->
            adv_items:predict_all(Ratings, Options)
    end;
recommend_all(RatingValues, Options) when is_list(RatingValues), is_list(Options) ->
    Ratings = adv_ratings:make_ratings(RatingValues),
    adv_items:predict_all(Ratings, Options).


% ~~ Declaration: Internal
%empty


% ~~ Implementation: API
%empty


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
%empty


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
