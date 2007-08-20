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
%%% @doc Mnesia initialization functions.
%%%
%%% @end
-module(adv_mnesia).

% ~~ Declaration: API
-export([
    init/1
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%% @spec init(PropertyList) -> ok | {error, Reason}
init(Options) ->
    Dir = proplists:get_value(dir, Options),
    ok = case mnesia:system_info(is_running) of
        yes ->
            case mnesia:system_info(directory) of
                Dir ->
                    ok;
                _ ->
                    {error, "mnesia application already running with different configuration"}
            end;
        no ->
            ?DEBUG("Starting mnesia", []),
            ok = filelib:ensure_dir(Dir),
            ok = application:set_env(mnesia, dir, Dir),
            mnesia:create_schema([node()]), %TODO may fail, don't care (already exists)?
            application:start(mnesia)
    end.


% ~~ Implementation: Internal
%empty
