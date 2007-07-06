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
    get_recommender/0
]).

% ~~ Declaration: Internal
%empty


% ~~ Implementation: API

%%% @doc  Get the recommender callback module from app environment.
%%% @spec () -> {Module::atom(), Options::list()}
%%% @end
get_recommender() ->
    case application:get_env(recommender) of
        {ok, Recommender={_Mod, _Options}} ->
            Recommender;
        _ ->
            {adv_slone, []}
    end.


% ~~ Implementation: Internal
%empty

