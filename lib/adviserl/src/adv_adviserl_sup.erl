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
%%% @doc Root adviserl supervisor.
%%%
%%% Manage a sources' ratings server, and a prediction server.
%%%
%%% @end
-module(adv_adviserl_sup).


% ~~ Declaration: OTP
-behaviour(supervisor).
-export([
    init/1
]).


% ~~ Declaration: API
-export([
    start_link/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%%% @doc  Start this main supervisor.
%%% The main supervisor is run and registered as local process.
%%% @spec () -> Result
%%% @see  supervisor:start_link/3
%%% @end
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


% ~~ Implementation: Behaviour callbacks

%%% @doc  Supervision of permanent servers: sources' ratings and items
%%% @spec (Args) -> {ok, {Policy, [Child]}}
%%% @see  supervisor:init/1
%%% @end
init(_Args) ->
    % 4 servers for the system
    ItemBehaviour = adv_config:get_items_behaviour(),
    {ItemsMod, ItemsModArgs} = ItemBehaviour,
    ?INFO("items' module configuration: ~w", [ItemBehaviour]),
    Items = {
        adv_items,
        {
            gen_server,
            start_link,
            [{local, ?ITEMS_PNAME}, ItemsMod, ItemsModArgs, []]
        },
        permanent,
        5000,
        worker,
        [ItemsMod]
    },
    SourceBehaviour = adv_config:get_sources_behaviour(),
    {SourcesMod, SourcesModArgs} = SourceBehaviour,
    ?INFO("sources' module configuration: ~w", [SourceBehaviour]),
    Sources = {
        adv_sources,
        {
            gen_server,
            start_link,
            [{local, ?SOURCES_PNAME}, SourcesMod, SourcesModArgs, []]
        },
        permanent,
        5000,
        worker,
        [SourcesMod]
    },
    RatingBehaviour = adv_config:get_ratings_behaviour(),
    {RatingsMod, RatingsModArgs} = RatingBehaviour,
    ?INFO("ratings' module configuration: ~w", [RatingBehaviour]),
    Ratings = {
        adv_ratings,
        {
            gen_server,
            start_link,
            [{local, ?RATINGS_PNAME}, RatingsMod, RatingsModArgs, []]
        },
        permanent,
        5000,
        worker,
        [RatingsMod]
    },
    PredBehaviour = adv_config:get_predictions_behaviour(),
    {PredMod, PredModArgs} = PredBehaviour,
    ?INFO("predictions' module configuration: ~w", [PredBehaviour]),
    % only one recommender is supported: if multiple are needed, a specific
    % gen_server module can be implemented, which integrate multiple
    % recommender algorithms as well as a merging mechanism.
    Predictions = {
        adv_predictions,
        {
            gen_server,
            start_link,
            [{local, ?PREDICTIONS_PNAME}, PredMod, PredModArgs, []]
        },
        permanent,
        5000,
        worker,
        [PredMod]
    },
    API = {
        adv_api,
        {adv_api, start_link, []},
        permanent,
        5000,
        worker,
        [adv_api]
    },
    % supervisor policy
    {ok, {
        {one_for_all, 2, 5},
        [
            Items, Sources, Ratings, Predictions,
            API
        ]
    }}.


% ~~ Implementation: Internal
%empty
