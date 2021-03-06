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
%%% @doc Utilities (loop functions, trace functions ...).
%%%
%%% @end
-module(adv_util).

% ~~ Declaration: API
-export([
    log/5,
    for_seq/3,
    for_seq/4,
    proplists_get_values/2,
    load_app_files/0,
    load_app_files/1,
    save_app_files/0,
    save_app_files/1,
    locate_files/2
]).

% ~~ Declaration: Internal
%empty


% ~~ Implementation: API

%%% @doc  Add information in the log streams.
%%% If Level is <em>dbg</em>, print message on <em>stdout</em>; else use the standard application <em>error_logger</em> (levels stands for info, warning and error).<br/>
%%% This function is used through applications macros (<em>adviserl.hrl</em>) which automaticaly capture <em>Level</em>, <em>Module</em> and <em>Line</em>.
%%% @spec (Module::atom(), Line::integer(), Level, Msg::string(), Params) -> integer()
%%%   Level = debug|dbg | normal|inf | warn|wrn | error|err
%%%   Params = [term()]
%%% @end
log(Module, Line, debug, Msg, Params) ->
    io:format(
        "Debug:~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, dbg, Msg, Params) ->
    io:format(
        "Debug:~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, normal, Msg, Params) ->
    error_logger:info_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, inf, Msg, Params) ->
    error_logger:info_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, warn, Msg, Params) ->
    error_logger:warning_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, wrn, Msg, Params) ->
    error_logger:warning_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, error, Msg, Params) ->
    error_logger:error_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    );
log(Module, Line, err, Msg, Params) ->
    error_logger:error_msg(
        "~p:~p: " ++ Msg ++ "~n",
        [Module, Line] ++ Params
    ).

%%% @doc  Apply a function on a sequence of items.
%%% Equiv for_seq(Body, Start, End, 1)
%%% @spec (BodyFun, Start, End) -> ok
%%% @end
for_seq(Body, Start, End) ->
    for_seq(Body, Start, End, 1).

%%% @doc  Apply a function on a sequence of items (items must be comparable
%%% and support the addition operator).
%%% Equivalent to lists:foreach(Body, lists:seq(Start, End, Incr)) but do not
%%% construct the iteration list.
%%% @spec (BodyFun, Start, End, Incr) -> ok
%%% @end
for_seq(Body, Start, End, Incr) ->
    case Start < End of
        true ->
            Body(Start),
            for_seq(Body, Start + Incr, End, Incr);
        _ ->
            ok
    end.

%% @spec proplists_get_values(Keys, Proplist) -> [Values] | {error, Reason}
%% @doc From a list of Keys return a list of values from Proplist.
%% If any of those Keys is not found, return error tuple.
proplists_get_values(Keys, Proplist) ->
    lists:foldl(
        fun
            (_Entry, Err = {error,_}) ->
                Err;
            (Entry, Acc) ->
                case proplists:get_value(Entry, Proplist) of
                    undefined ->
                        {error, io_lib:format("key ~w not found", [Entry])};
                    Value ->
                        [Value|Acc]
                end
        end,
        [],
        lists:reverse(Keys)
    ).

%% @spec load_app_files() -> ok | {error, Reason::string()}
%%
%% @doc Read backup data from files specified in configuration.
%%
%% WARNING: may result in inconsistent system if any server is processing
%% other request during the loading.
load_app_files() ->
    apply_to_files_spec(fun load_app_files/1).

%% @spec load_app_files(FilesSpec) -> ok | {error, Reason}
%%
%% @doc Read backup data from files.
load_app_files(Spec = {_Items, _Sources, _Ratings, _Predictions, _Options}) ->
    %TODO if any of them failed, the whole system become inconsistent!
    %TODO the whole system become inconsistent during the load!!!
    %TODO nothing is done to protect the system: the whole system may become inconsistent if it receive other request during the load (servers are concurrent)
    %solution? all server should have a pause state, so that everybody is set to pause before to load, then resume afterward
    apply_to_files(Spec, load_file). %TODO for each server!

%% @spec save_app_files() -> ok | {error, Reason::string()}
%%
%% @doc Write backup data into files specified in configuration.
%%
%% WARNING: may result in inconsistent system if any server is processing
%% other request during the save.
save_app_files() ->
    apply_to_files_spec(fun save_app_files/1).

%% @spec save_app_files(FilesSpec) -> ok | {error, Reason::string()}
%%
%% @doc Write backup data into files.
save_app_files(Spec = {_Items, _Sources, _Ratings, _Predictions, _Options}) ->
    %TODO if any of them failed, the whole system become inconsistent! and may crash previous state also!!!
    %solution?: using a temporary folder to save then copy in real one if all is ok.
    %TODO nothing is done to protect the system: the whole system may become inconsistent if it receive other request during the save (servers are concurrent)
    %solution? all server should have a pause state, so that everybody is set to pause before to save, then resume afterward
    apply_to_files(Spec, save_file). %TODO for each server!

%% @spec locate_files([File], [Option]) -> {ok, [AbsFile]}|{error,Reason}
%%     File = {relative_file, Filename::string()} |
%%            {absolute_file, Filename::string()}
%%     Option = {dir, Path::string()}
%%
%% @doc Locate all files or return error.
locate_files(Files, Options) ->
    case locate_dir(Options) of
        Err1 = {error, _} ->
            Err1;
        Path ->
            lists:foldl(
                fun
                    (_File, Err2 = {error, _Reason}) ->
                        Err2;
                    (File, {ok, Acc}) ->
                        case locate_file(File, Path) of
                            Err3 = {error, _} ->
                                Err3;
                            AbsFile ->
                                {ok, Acc ++ [AbsFile]}
                        end
                end,
                {ok, []},
                Files
            )
    end.


% ~~ Implementation: Internal

%% @spec locate_dir([Option]) -> Dir::string() | {error, Reason}
%%     Option = {dir, Dir::string()}
%% @doc Find absolute dir name and ensure it exists.
locate_dir(Options) ->
    case proplists:get_value(dir, Options) of
        undefined ->
            case file:get_cwd() of
                {ok, Dir} ->
                    ensure_absdir(Dir);
                _ ->
                    {error, "No specified folder and CWD not accessible."}
            end;
        Dir2 ->
            ensure_absdir(Dir2)
    end.

ensure_absdir(Dir) ->
    AbsDir = filename:absname(Dir),
    case filelib:ensure_dir(AbsDir) of
        ok ->
            AbsDir;
        {error, Reason} ->
            {error, io_lib:format(
                "Directory '~s' not accessible: ~w",
                [AbsDir, Reason]
            )}
    end.

%% @spec locate_file(File, Path::string()) -> Absfile::string()|{error,Reason}
%%     File = {relative_file, Filename::string()} |
%%            {absolute_file, Filename::string()}
%% @doc Find file, ensure dir exists and file writable (create it if needed).
locate_file({relative_file, File}, Path) ->
    locate_file({absolute_file, filename:join(Path, File)}, Path);
locate_file({absolute_file, File}, _Path) ->
    case filelib:ensure_dir(File) of
        ok ->
            case file:open(File, [read, write]) of
                {ok, IODev} ->
                    case file:close(IODev) of
                        ok ->
                            File;
                        {error, Reason3} ->
                            {error, io_lib:format(
                                "Opened file '~s' can not be closed: ~w",
                                [File, Reason3]
                            )}
                    end;
                {error, Reason2} ->
                    {error, io_lib:format(
                        "File '~s' can not be open (rw): ~w",
                        [File, Reason2]
                    )}
            end;
        {error, Reason1} ->
            {error, io_lib:format(
                "Cannot ensure directory for '~s': ~w",
                [File, Reason1]
            )}
    end.

apply_to_files_spec(FilesFun) ->
    case adv_config:get_data_files_spec() of
        {ok, FilesSpec} ->
            FilesFun(FilesSpec);
        Err = {error, _Reason} ->
            Err
    end.

apply_to_files({Items, Sources, Ratings, Predictions, Options}, ModFun) ->
    Located = adv_util:locate_files(
        [Items, Sources, Ratings, Predictions],
        Options
    ),
    case Located of
        {ok, Files=[_IF, _SF, _RF, _PF]} ->
            lists:foldl(
                fun
                    ({_Mod, _File}, Err = {error, _Reason}) ->
                        Err;
                    ({Mod, File}, ok) ->
                        Mod:ModFun(File, Options)%TODO return ok|{error,R}
                end,
                ok,
                lists:zip(
                    [adv_items, adv_sources, adv_ratings, adv_predictions],
                    Files
                )
            );
        Err ->
            Err
    end.

