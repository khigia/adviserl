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
%%% @doc Store of data object (advdata) in Mnesia table.
%%%
%% @todo Use a mnesia counter instead of using state data (distribution)
%%% @end
-module(adv_data_mnesia).


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
    table = undefined,
    next = undefined
}).


% ~~ Implementation: Behaviour callbacks

init([TableName]) ->
    NextID = init_table(TableName),
    {ok, #st{
        table = TableName,
        next = NextID
    }}.

handle_call(info, _From, State) ->
    {reply, [
        {module, ?MODULE},
        {table_name, State#st.table},
        {table_version, mnesia:table_info(State#st.table, version)},
        {table_size, mnesia:table_info(State#st.table, size)},
        {next_id, State#st.next}
    ], State};

handle_call({load_file, _File, _Options}, _From, State) ->
    {reply, {error, "no implementation: use mnesia backup"}, State};

handle_call({save_file, _File, _Options}, _From, State) ->
    {reply, {error, "no implementation: use mnesia backup"}, State};

handle_call({insert_new, Key, Data}, _From, State = #st{table=TableName}) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, {Result, NewState}} = mnesia:transaction(fun() ->
        AdvData = mnesia:index_read(TableName, Key, #advdata.key),
        case AdvData of
            [#advdata{id=ID}] ->
                {{ok, false, ID}, State};
            _ ->
                NewID = State#st.next,
                NewRecord = #advdata{id = NewID, key = Key, data = Data},
                mnesia:write(TableName, NewRecord, write),
                {{ok, true, NewID}, State#st{next = NewID + 1}}
        end
    end),
    {reply, Result, NewState};

handle_call({id_from_key, Key}, _From, State = #st{table=TableName}) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:index_read(TableName, Key, #advdata.key) of
            [#advdata{id=ID}] ->
                ID;
            _ ->
                undefined
        end
    end),
    {reply, Result, State};

handle_call({object_from_key, Key}, _From, State = #st{table=TableName}) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:index_read(TableName, Key, #advdata.key) of
            [AdvData] ->
                {ok, AdvData};
            _ ->
                undefined
        end
    end),
    {reply, Result, State};

handle_call({key_from_id, ID}, _From, State = #st{table=TableName}) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(TableName, ID, read) of
            [#advdata{key=Key}] ->
                {ok, Key};
            _ ->
                undefined
        end
    end),
    {reply, Result, State};

handle_call({object_from_id, ID}, _From, State = #st{table=TableName}) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Result} = mnesia:transaction(fun() ->
        case mnesia:read(TableName, ID, read) of
            [AdvData] ->
                {ok, AdvData};
            _ ->
                undefined
        end
    end),
    {reply, Result, State};

handle_call({fold, Fun, Acc}, _From, State = #st{table=TableName}) ->
    F = fun(#advdata{id = ID, key = Key, data = Data}, Acc0) ->
        Fun(ID, Key, Data, Acc0)
    end,
    {atomic, Result} = mnesia:transaction(fun() ->
        mnesia:foldl(F, Acc, TableName)
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

init_table(TableName) ->
    %TODO should not try to create it at each start (distributed app)
    Status = mnesia:create_table(TableName, [
        % distribution properties
        {ram_copies, [node()]},
        % table(data) properties
        {record_name, advdata},
        {type, set},
        {attributes, record_info(fields, advdata)},
        {index, [key]}
    ]),
    Prep = fun() ->
        ok = mnesia:wait_for_tables([TableName], 20000),
        {atomic, MaxID} = mnesia:transaction(fun() ->
            QH = qlc:q([R#advdata.id || R <- mnesia:table(TableName)]),
            lists:max([0 | qlc:e(QH)])
        end),
        MaxID + 1
    end,
    case Status of
        {atomic,ok} ->
            ?DEBUG("table '~w' created", [TableName]),
            Prep();
        {aborted,{already_exists,TableName}} ->
            ?DEBUG("using existing table '~w'", [TableName]),
            Prep();
        _ ->
            ?ERROR("cannot create table '~w'", [TableName], Status),
            undefined
    end.


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
