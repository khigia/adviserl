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
%%% @doc Mnesia storage of ratings.
%%%
%%% Refer to {@link adv_types} for type definitions.
%%%
%%% @end
-module(adv_ratings_mnesia).


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
-export([
    from_list/1,
    to_list/1,
    fold_ratings/3
]).
-export([
    print_debug/0
]).


% ~~ Declaration: Internal

-include_lib("stdlib/include/qlc.hrl").

-include("include/adviserl.hrl").

-record(st, {
    table
}).


% ~~ Implementation: API

%%% @doc  Create a ratings structure from a list of key-value pairs.
%%% @spec ([{itemID(),ratingValue()}]) -> ratings()
%%% @todo Check for errors
%%% @end
from_list(RatingKeyValues) when is_list(RatingKeyValues) ->
    lists:map(
        fun({Key,Val}) -> {Key, {Val, nodata}} end,
        RatingKeyValues
    ).

%%% @doc  Create a list of key-value pairs from a ratings structure.
%%% @spec (ratings()) -> [{itemID(),ratingValue()}]
%%% @todo Check for errors
%%% @end
to_list(Ratings) ->
    lists:map(
        fun({Key, {RatingValue, _RatingData}}) -> {Key, RatingValue} end,
        Ratings
    ).

%%% @doc  Fold a ratings structure passing rating by rating to the function.
%%% @spec (((itemID(),rating(),Acc)->Acc), Acc, ratings()) -> Acc
%%% @end
fold_ratings(Fun, Accumulator, SourceRatings) ->
    lists:foldl(
        fun({ItemID, {Score,Data}}, Acc0) ->
            Fun(ItemID, {Score,Data}, Acc0)
        end,
        Accumulator,
        SourceRatings
    ).

%%% @doc  Print the full matrix in debug mode.
%%% @spec () -> ok
%%% @end
print_debug() ->
    RatingsToList = fun(Ratings) ->
        List0 = fold_ratings(
            fun(ItemID, {Val,_Dta}, Acc) ->
                Acc ++ [{ItemID,Val}]
            end,
            [],
            Ratings
        ),
        List1 = lists:keysort(2, List0),
        List1
    end,
    Printer = fun(SourceID, Ratings, Acc) ->
        io:format("src ~w: ~w~n", [SourceID, RatingsToList(Ratings)]),
        Acc
    end,
    adv_ratings:fold_sources(Printer, ok).


% ~~ Implementation: Behaviour callbacks

init([TableName]) ->
    State = #st{
        table = init_table(TableName)
    },
    {ok, State}.

handle_call({load_file, _File, _Options}, _From, State) ->
    % mnesia automaticaly restore dumps
    {reply, ok, State};

handle_call({save_file, _File, _Options}, _From, State) ->
    % simple dump
    TableName = State#st.table,
    {reply, mnesia:dump_tables([TableName]), State};

handle_call({set_rating, SourceID, ItemID, Rating}, _From, State) ->
    TableName = State#st.table,
    R = set_rating(TableName, SourceID, ItemID, Rating),
    {reply, R, State};

handle_call({get_rating, SourceID, ItemID}, _From, State) ->
    TableName = State#st.table,
    R = get_rating(TableName, SourceID, ItemID),
    {reply, R, State};

handle_call({update_rating, SourceID, ItemID, Updater, Default}, _From, State) ->
    TableName = State#st.table,
    R = update_rating(TableName, SourceID, ItemID, Updater, Default),
    {reply, R, State};

handle_call({get_ratings, SourceID}, _From, State) ->
    TableName = State#st.table,
    RR = get_ratings(TableName, SourceID),
    {reply, RR, State};

handle_call({fold_sources, Fun, Acc}, From, State) ->
    TableName = State#st.table,
    spawn(fun() ->
        R = fold_sources(TableName, Fun, Acc),
        gen_server:reply(From, R)
    end),
    {noreply, State};

handle_call(stop, _From, State) ->
    {stop, "Stop requested", ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

%%% @spec init_table(TableName::atom()) -> TableName::atom()
%%% @doc  Create table containing all ratings of all sources.
%%% @private
%%% @end
init_table(TableName) ->
    %TODO should not try to create it at each start (distributed app)
    Status = mnesia:create_table(TableName, [
        % distribution properties
        {ram_copies, [node()]},
        % table(data) properties
        {record_name, advrating},
        {type, bag},
        {attributes, record_info(fields, advrating)},
        {index, [item]}
    ]),
    Prepare = fun() ->
        ok = mnesia:wait_for_tables([TableName], 20000),
        TableName
    end,
    case Status of
        {atomic,ok} ->
            ?DEBUG("table '~w' created", [TableName]),
            Prepare();
        {aborted,{already_exists,TableName}} ->
            ?DEBUG("using existing table '~w'", [TableName]),
            Prepare();
        _ ->
            ?ERROR("cannot create table '~w'", [TableName], Status),
            undefined
    end.

%%% @spec get_rating(TableName, sourceID(), itemID()) -> rating()|undefined
%%% @doc  Retrieve one rating for one source-item pair.
%%% @private
get_rating(TableName, SourceID, ItemID) ->
    % following may fail but this is a system failure,
    % should not be send back to user
    % ... supervisor should handle
    {atomic, Reply} = mnesia:transaction(fun() ->
        QH = qlc:q([{R#advrating.score, R#advrating.data} ||
            R <- mnesia:table(TableName),
            R#advrating.source =:= SourceID,
            R#advrating.item =:= ItemID
        ]),
        case qlc:e(QH) of
            [] ->
                undefined;
            [Rating] ->
                Rating;
            _ ->
                {error, "inconsistent DB state"}
        end
    end),
    Reply.

%%% @spec set_rating(TableName, sourceID(), itemID(), rating()) -> ok
%%% @doc  Add or modify one rating.
%%% @private
set_rating(TableName, SourceID, ItemID, Rating) ->
    update_rating(
        TableName,
        SourceID,
        ItemID,
        fun(_Previous) -> Rating end,
        Rating
    ).

%%% @spec update_rating(TableName, sourceID(), itemID(), (rating())->rating(), rating()) -> ok
%%% @doc  Update a rating.
%%% @private
update_rating(TableName, SourceID, ItemID, Updater, _Default={Score,Data}) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        QH = qlc:q([R ||
            R <- mnesia:table(TableName),
            R#advrating.source =:= SourceID,
            R#advrating.item =:= ItemID
        ]),
        case qlc:e(QH) of
            [] ->
                Record = #advrating{
                    source = SourceID,
                    item = ItemID,
                    score = Score,
                    data = Data
                },
                mnesia:write(TableName, Record, write);
            [Record] ->
                {NewScore, NewData} = Updater(
                    {Record#advrating.score, Record#advrating.data}
                ),
                NewRecord = Record#advrating{
                    score = NewScore,
                    data = NewData
                },
                ok = mnesia:delete_object(TableName, Record, write),
                mnesia:write(TableName, NewRecord, write);
            _ ->
                {error, "inconsistent DB state"}
        end
    end),
    Reply.

%%% @spec get_ratings(TableName, sourceID()) -> ratings()|undefined
%%% @doc  Retrieve all ratings for one source.
%%% @private
get_ratings(TableName, SourceID) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        QH = qlc:q([
            {R#advrating.item, {R#advrating.score, R#advrating.data}} ||
            R <- mnesia:table(TableName),
            R#advrating.source =:= SourceID
        ]),
        case qlc:e(QH) of
            [] ->
                undefined;
            Ratings ->
                Ratings
        end
    end),
    Reply.

%%% @spec fold_sources(TableName, Fun::(sourceID(), ratings(), Acc)->Acc, Acc) -> Acc
%%% @doc  Fold for each set of ratings.
%%% @private
fold_sources(TableName, Fun, Acc) ->
    {atomic, Reply} = mnesia:transaction(fun() ->
        Sources = mnesia:all_keys(TableName),
        lists:foldl(
            fun(Source, Acc0) ->
                Fun(Source, get_ratings(TableName, Source), Acc0)
            end,
            Acc,
            Sources
        )
    end),
    Reply.


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").

%%% @hidden
pub_init() ->
    MnesiaConfig = adv_config:get_mnesia_config(),
    ok = adv_mnesia:init(MnesiaConfig),
    gen_server:start_link(
        {local, ?RATINGS_PNAME},
        ?MODULE,
        [tmptbl],
        []
    ).

%%% @hidden
pub_setget_rating_test() ->
    {ok, Pid} = pub_init(),
    adv_ratings:set_rating(1,2,{3,some_data}),
    ?assert(adv_ratings:get_rating(1,2) == {3, some_data}),
    ?assert(adv_ratings:get_rating(1,4) == undefined),
    ?assert(adv_ratings:get_rating(4,2) == undefined),
    gen_server:call(Pid, stop).

-endif.
