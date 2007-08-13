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
%%% @doc Store of data objects (advdata) in ETS.
%%%
%%% @end
-module(adv_data_ets).


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
%empty (intend to be called by adv_items or adv_sources API)


% ~~ Declaration: Internal

-include("include/adviserl.hrl").

-record(st, {
    tid,         % ETS identifier containing #advdata records
    index,       % gb_tree index per ID (ETS is indexed per Key)
    next         % next free ID slot
}).


% ~~ Implementation: Behaviour callbacks

init([TableName]) ->
    {ok, #st{
        tid = ets:new(TableName, [set, {keypos,#advdata.key}]),
        index = gb_trees:empty(),
        next = 1
    }}.

handle_call({load_file, File, _Options}, _From, State) ->
    case dets:open_file(list_to_atom(File), [{file, File}]) of
        {ok, Dets} ->
            TID = State#st.tid,
            ets:delete_all_objects(TID),
            ets:from_dets(TID, Dets),
            dets:close(Dets),
            {Idx, Cur} = ets:foldl(
                fun(#advdata{id=ID, key=Key}, {Tree, Pos}) ->
                    {
                        gb_trees:enter(ID, Key, Tree),
                        lists:max([Pos, ID + 1])
                    }
                end,
                {gb_trees:empty(), 1},
                TID
            ),
            {reply, ok, State#st{index=Idx, next=Cur}};
        Err1 ->
            {reply, Err1, State}
    end;
handle_call({save_file, File, _Options}, _From, State) ->
    file:delete(File),
    case dets:open_file(list_to_atom(File), [{file, File}]) of
        {ok, Dets} ->
            TID = State#st.tid,
            case dets:from_ets(Dets, TID) of
                ok ->
                    dets:close(Dets),
                    {reply, ok, State};
                Err2 ->
                    dets:close(Dets),
                    {reply, Err2, State}
            end;
        Err1 ->
            {reply, Err1, State}
    end;
handle_call({insert_new, Key, Data}, _From, State) ->
    TID = State#st.tid,
    Idx = State#st.index,
    Cur = State#st.next,
    case ets:insert_new(TID, #advdata{id=Cur, key=Key, data=Data}) of
        true ->
            NewIdx = gb_trees:enter(Cur, Key, Idx),
            {reply, {ok, true, Cur}, State#st{index=NewIdx, next=Cur + 1}};
        _ ->
            KnownID = ets:lookup_element(TID, Key, #advdata.id),
            {reply, {ok, false, KnownID}, State}
    end;
handle_call({id_from_key, Key}, _From, State) ->
    TID = State#st.tid,
    case (catch ets:lookup_element(TID, Key, #advdata.id)) of
        Int when is_integer(Int) ->
            {reply, Int, State};
        _ ->
            {reply, undefined, State}
    end;
handle_call({object_from_key, Key}, _From, State) ->
    TID = State#st.tid,
    case ets:lookup(TID, Key) of
        [Object] ->
            {reply, {ok, Object}, State};
        [] ->
            {reply, undefined, State}
    end;
handle_call({key_from_id, ID}, _From, State) ->
    Idx = State#st.index,
    case gb_trees:lookup(ID, Idx) of
        {value, Key} ->
            {reply, {ok, Key}, State};
        _ ->
            {reply, undefined, State}
    end;
handle_call({object_from_id, ID}, From, State) ->
    Idx = State#st.index,
    case gb_trees:lookup(ID, Idx) of
        {value, Key} ->
            handle_call({object_from_key, Key}, From, State);
        _ ->
            {reply, undefined, State}
    end;
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
%nothing


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
