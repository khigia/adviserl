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
    init/0,
    backup/0
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
            R = application:start(mnesia),
            restore(),
            R
    end.

%% @spec backup() -> ok | {error, Reason}
backup() ->
    MnesiaConfig = adv_config:get_mnesia_config(),
    BackupConfig = proplists:get_value(backup, MnesiaConfig),
    case BackupConfig of
        undefined ->
            {error, io_lib:format(
                "no backup defined in application environment: ~w",
                [MnesiaConfig]
            )};
        Backup ->
            case adv_util:proplists_get_values([file, tables], Backup) of
                [File, Tables] ->
                    backup(File, Tables);
                Err = {error, _} ->
                    Err
            end
    end.


% ~~ Implementation: Internal

backup(File, Tables) ->
    {ok, Name, _Nodes} = mnesia:activate_checkpoint([
        {max, Tables},
        {ram_overrides_dump,true}
    ]),
    R = mnesia:backup_checkpoint(Name, File),
    ok = mnesia:deactivate_checkpoint(Name),
    R.

restore() ->
    % not exported, because dangerous of restoring data while services are running (at least each service need to update its state, and synchrnization is required)
    % restore is thus done only at startup, before adviserl services run
    MnesiaConfig = adv_config:get_mnesia_config(),
    BackupConfig = proplists:get_value(backup, MnesiaConfig),
    case BackupConfig of
        undefined ->
            ?INFO("No backup to restore", []),
            ok;
        Backup ->
            case adv_util:proplists_get_values([file, tables], Backup) of
                [File, Tables] ->
                    % testing the file may avoid to wait for tables
                    case filelib:is_file(File) of
                        true ->
                            ok = mnesia:wait_for_tables(Tables, 5000),
                            St = mnesia:restore(File, [{recreate_tables, Tables}]),
                            ?INFO("Backup '~s' restore status:~w", [File, St]),
                            St;
                        _ ->
                            ?WARNING("Cannot reach backup file:~s", [File]),
                            {error, not_a_file}
                    end;
                Err = {error, _} ->
                    ?WARNING("Backup definition cannot be restored:~w", [Err]),
                    Err
            end
    end.

