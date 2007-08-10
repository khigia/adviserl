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
%%% @doc Implementation of adv_souces using Mnesia.
%%%
%%% @end
-module(adv_sources_mnesia).


% ~~ Declaration: OTP relative
-behaviour(gen_server).
-export([
    init/1,         % (InitArgs) -> Result
    handle_call/3,  % (Request, From, State) -> Result
    handle_cast/2,  % (Request, State) -> Result
    handle_info/2,  % (Info, State) -> Result
    terminate/2,    % (Reason, State) -> term() % result is not used
    code_change/3   % (OldVsn, State, Extra) -> {ok, NewState}
]).


% ~~ Declaration: API
%empty


% ~~ Declaration: Internal

-include("include/adviserl.hrl").

-record(st, {
    sources_table
}).


% ~~ Implementation: Behaviour callbacks

init(Options) ->
    SourcesTbl = init_sources_table(
        proplists:get_value(sources_table, Options)
    ),
    {ok, #st{
        sources_table = SourcesTbl
    }}.

handle_call({load_file, File, _Options}, _From, State) ->
    {reply, {error, not_implemented}, State};
handle_call({save_file, File, _Options}, _From, State) ->
    {reply, {error, not_implemented}, State};
handle_call({insert_new, Key, Data}, _From, State) ->
    {reply, {error, not_implemented}, State};
handle_call({id_from_key, Key}, _From, State) ->
    {reply, {error, not_implemented}, State};
handle_call({object_from_key, Key}, _From, State) ->
    {reply, {error, not_implemented}, State};
handle_call({key_from_id, ID}, _From, State) ->
    {reply, {error, not_implemented}, State};
handle_call({object_from_id, ID}, From, State) ->
    {reply, {error, not_implemented}, State};
handle_call(_Request, _From, State) ->
    {reply, unknown_call, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

init_sources_table(undefined) ->
    init_sources_table([]);
init_sources_table(_Options) ->
    %TODO should not try to create it at each start (distribuited app)
    %TODO table creation should have a special call (not init)???
    mnesia:create_table(advsrc, [
        {attributes, record_info(fields, advsrc)}
    ]).


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
