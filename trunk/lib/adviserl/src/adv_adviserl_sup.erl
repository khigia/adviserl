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
%empty


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
    RatingsSrv = {
        adv_ratings,
        {adv_ratings, start_link, []},
        permanent,
        5000,
        worker,
        [adv_ratings]
    },
    Recommender = adv_config:get_recommender(),
    ItemsSrv = {
        adv_items,
        {adv_items, start_link, [Recommender]},
        permanent,
        5000,
        worker,
        [adv_items]
    },
    % supervisor policy
    {ok, {
        {one_for_all, 2, 5},
        [RatingsSrv, ItemsSrv]
    }}.


% ~~ Implementation: Internal
%empty