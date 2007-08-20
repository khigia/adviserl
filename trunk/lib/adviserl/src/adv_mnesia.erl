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
    init/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%% @spec init() -> ok | {error, Reason}
init() ->
    MnesiaConfig = adv_config:get_mnesia_config(),
    Dir = filename:absname(proplists:get_value(dir, MnesiaConfig)),
    ok = case mnesia:system_info(is_running) of
        yes ->
            MnesiaDir = filename:absname(mnesia:system_info(directory)),
            case MnesiaDir of
                Dir ->
                    ok;
                _ ->
                    {error, io_lib:format(
                        "mnesia application already running with different directory configuration [expected: ~s, got:~s]",
                        [Dir, MnesiaDir]
                    )}
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
