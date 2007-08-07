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
%%% @doc Configuration management for adviserl application
%%%
%%% @end
-module(adv_config).

% ~~ Declaration: API
-export([
    get_sources_behaviour/0,
    get_items_behaviour/0,
    get_ratings_behaviour/0,
    get_ratings_module/0,
    get_predictions_behaviour/0,
    get_data_files_spec/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%% @spec get_sources_behaviour() -> {Module::atom(), Options::list()}
%%
%% @doc  Get the sources module and config from app environment.
%%
%% Default value is {adv_data_ets, [adv_sources]}.
%% @see get_behaviour/1
%% @end
get_sources_behaviour() ->
    get_behaviour(sources, {adv_data_ets, [adv_sources]}).

%% @spec get_items_behaviour() -> {Module::atom(), Options::list()}
%%
%% @doc  Get the items module and config from app environment.
%%
%% Default value is {adv_data_ets, [adv_items]}.
%% @see get_behaviour/1
%% @end
get_items_behaviour() ->
    get_behaviour(items, {adv_data_ets, [adv_items]}).

%% @spec get_ratings_behaviour() -> {Module::atom(), Options::list()}
%%
%% @doc  Get the ratings module and config from app environment.
%%
%% Default value is {adv_ratings_dod, []}.
%% @see get_behaviour/1
%% @end
get_ratings_behaviour() ->
    get_behaviour(ratings, {adv_ratings_dod, []}).

%% @spec get_ratings_module() -> Module::atom()
%%
%% @doc  Get the ratings module from app environment.
%% @see get_ratings_behaviour/0
%% @end
get_ratings_module() ->
    {Mod, _} = get_ratings_behaviour(),
    Mod.

%% @spec get_predictions_behaviour() -> {Module::atom(), Options::list()}
%%
%% @doc  Get the recommender module and config from app environment.
%%
%% Default value is {adv_slone_smdod, []}.
%% @see get_behaviour/1
%% @end
get_predictions_behaviour() ->
    get_behaviour(predictions, {adv_slone_smdod, []}).

%% @spec get_data_files_spec() -> {ok, Spec} | {error, Reason}
%%
%% @doc  Get the files spec from app environment.
get_data_files_spec() ->
    case application:get_env(adviserl, data_files) of
        R = {ok, {_Items, _Source, _Ratings, _Predictions, _Options}} ->
            R;
        Any ->
            {error, "Key 'data_files' not found in app config"}
    end.
            

% ~~ Implementation: Internal

%% @spec get_behaviour(Key::atom(), Default::{atom(),list()}) -> {Module::atom(), Options::list()}
%%
%% @doc  Get/set a module and config for a given Key in app environment.
%%
%% Return the tuple {Mod, Args} configured by key 'Key'. 'Mod' is the
%% (gen_server) module to be used, 'Args' is a list of arguments given when
%% starting the behaviour.
%% If Key is not found and application is defined, use Default and set this
%% value in app environment.
get_behaviour(Key, Default) ->
    case application:get_env(Key) of
        {ok, Result={_Mod, _Options}} ->
            Result;
        _ ->
            case application:get_application() of
                {ok, App} ->
                    application:set_env(App, Key, Default),
                    Default;
                _ ->
                    Default
            end
    end.
