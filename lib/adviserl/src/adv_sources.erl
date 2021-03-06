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
%%% @copyright 2007 Affle Pvt. Ltd.
%%% @author Ludovic Coquelle <lcoquelle@gmail.com>
%%% @doc API to adv_data for sources.
%%%
%%% @end
-module(adv_sources).


% ~~ Declaration: API
-export([
    info/0,
    load_file/2,
    save_file/2,
    insert_new/2,
    id_from_key/1,
    object_from_key/1,
    key_from_id/1,
    object_from_id/1
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

info() ->
    CommonInfo = [
        {process_name, ?SOURCES_PNAME},
        {module_spec, adv_config:get_sources_behaviour()}
        %{whereis, erlang:whereis(?SOURCES_PNAME)}
    ],
    MaybeInfo = (catch gen_server:call(
        ?SOURCES_PNAME,
        info
    )),
    case MaybeInfo of
        {'EXIT', _} ->
            CommonInfo;
        Info ->
            CommonInfo ++ Info
    end.

load_file(File, Options) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {load_file, File, Options}
    ),
    ?INFO("file loaded", []).

save_file(File, Options) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {save_file, File, Options}
    ),
    ?INFO("file saved", []).

insert_new(Key, Data) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {insert_new, Key, Data}
    ).

%% @spec id_from_key(Key::term()) -> ID::integer() | undefined
id_from_key(Key) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {id_from_key, Key}
    ).

object_from_key(Key) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {object_from_key, Key}
    ).

key_from_id(ID) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {key_from_id, ID}
    ).

object_from_id(ID) ->
    gen_server:call(
        ?SOURCES_PNAME,
        {object_from_id, ID}
    ).


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
