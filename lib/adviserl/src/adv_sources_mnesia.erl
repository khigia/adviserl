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

-include_lib("stdlib/include/qlc.hrl").

-record(st, {
    next = undefined
}).


% ~~ Implementation: Behaviour callbacks

init(Options) ->
    NextID = init_sources_table(Options),
    {ok, #st{
        next = NextID
    }}.

handle_call({load_file, _File, _Options}, _From, State) ->
    % mnesia automaticaly restore dumps
    {reply, ok, State};
handle_call({save_file, _File, _Options}, _From, State) ->
    % simple dump
    {reply, mnesia:dump_tables([advsrc]), State};
handle_call({insert_new, Key, Data}, _From, State) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, {Result, NewState}} = mnesia:transaction(fun() ->
        Src = mnesia:index_read(advsrc, Key, #advsrc.key),
        case Src of
            [#advsrc{id=SrcID}] ->
                {{ok, false, SrcID}, State};
            _ ->
                NewID = State#st.next,
                mnesia:write(#advsrc{id = NewID, key = Key, data = Data}),
                {{ok, true, NewID}, State#st{next = NewID + 1}}
        end
    end),
    {reply, Result, NewState};
handle_call({id_from_key, Key}, _From, State) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:index_read(advsrc, Key, #advsrc.key) of
            [#advsrc{id=SrcID}] ->
                SrcID;
            _ ->
                undefined
        end
    end),
    {reply, Result, State};
handle_call({object_from_key, Key}, _From, State) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:index_read(advsrc, Key, #advsrc.key) of
            [Source] ->
                {ok, Source};
            _ ->
                undefined
        end
    end),
    {reply, Result, State};
handle_call({key_from_id, ID}, _From, State) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(advsrc, ID, read) of
            [#advsrc{key=Key}] ->
                {ok, Key};
            _ ->
                undefined
        end
    end),
    {reply, Result, State};
handle_call({object_from_id, ID}, From, State) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(advsrc, ID, read) of
            [Source] ->
                {ok, Source};
            _ ->
                undefined
        end
    end),
    {reply, Result, State};
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

init_sources_table(_Options) ->
    %TODO should not try to create it at each start (distributed app)
    Status = mnesia:create_table(advsrc, [
        % distribution properties
        {ram_copies, [node()]},
        % table(data) properties
        {type, set},
        {attributes, record_info(fields, advsrc)},
        {index, [key]}
    ]),
    Prep = fun() ->
        {atomic, MaxID} = mnesia:transaction(fun() ->
            ok = mnesia:wait_for_tables([advsrc], 20000),
            QH = qlc:q([R#advsrc.id || R <- mnesia:table(advsrc)]),
            lists:max([0 | qlc:e(QH)])
        end),
        MaxID + 1
    end,
    case Status of
        {atomic,ok} ->
            ?DEBUG("table advsrc created", []),
            Prep();
        {aborted,{already_exists,advsrc}} ->
            ?DEBUG("using existing table advsrc", []),
            Prep();
        _ ->
            ?ERROR("cannot create table advsrc", [], Status),
            undefined
    end.


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
