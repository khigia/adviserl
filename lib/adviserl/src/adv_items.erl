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
%%% @doc Management of cross-references.
%%%
%%% @end
-module(adv_items).


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
    start_link/1,
    init/0,
    update_rating/4,
    predict_all/2
]).
-export([
    print_debug/0
]).


% ~~ Declaration: Internal

-record(state, {
    callback, % module implementing the recommender algorithm (callback module)
    opaque    % opaque data structure of the callback module
}).

-include("include/adviserl.hrl").


% ~~ Implementation: API

%%% @doc  Start and localy register the server.
%%% @spec ({RecommenderModule, Options}) -> {ok, Pid} | ignore | {error, Error}
%%% @end
start_link(Recommender) ->
    %TODO this recommender could be passed as module parameters
    %(this could make it faster? but is less flexible for now)
    gen_server:start_link({local, ?MODULE}, ?MODULE, Recommender, []).

%%% @doc  Init the server by reloading the whole ratings collection.
%%% @spec () -> ok
%%% @todo remove infinity timeout
%%% @end
init() ->
    gen_server:call(?MODULE, init, infinity).

%%% @doc  Update rating influence on the recommender algorithm.
%%% @spec (sourceID(), itemID(), NewRating::rating(), OldRatings::ratings()|undefined) -> ok
%%% @todo remove infinity timeout
%%% @end
update_rating(SourceID, ItemID, NewRating, OldRatings)
    when
        is_integer(SourceID),
        SourceID > 0,
        is_integer(ItemID),
        ItemID > 0
    ->
    gen_server:call(
        ?MODULE,
        {update_rating, SourceID, ItemID, NewRating, OldRatings},
        infinity
    ).

%%% @doc  For a given set of ratings, give prediction for all items.
%%% @see  adviserl:recommend_all/2
%%% @spec (ratings(), [Option]) -> predictions()
%%% @todo remove infinity timeout
%%% @end
predict_all(Ratings, Options) ->
    gen_server:call(?MODULE, {predict_all, Ratings, Options}, infinity).

%%% @doc  Print the full matrix in debug mode.
%%% @spec () -> ok
%%% @todo remove infinity timeout
%%% @end
print_debug() ->
    gen_server:call(?MODULE, print_debug, infinity).


% ~~ Implementation: Behaviour callbacks

init({RecommenderCBModule, Options}) ->
    % only one recommender is supported: if multiple are needed, a specific
    % callback module can be implemented, wich integrate the real recommenders
    % (this put the merge behaviour outside of this module)
    RecommenderCBData = RecommenderCBModule:create_state(Options),
    State = #state{
        callback = RecommenderCBModule,
        opaque   = RecommenderCBData
    },
    {ok, State}.

handle_call(
    init,
    _From,
    State = #state{callback = Mod, opaque = _Data0}
) ->
    Data1 = Mod:load_ratings(),
    {reply, ok, State#state{opaque = Data1}};
handle_call(
    {update_rating, SourceID, ItemID, NewRating, OldRatings},
    From,
    State = #state{callback = Mod, opaque = Data0}
) ->
    Data1 = Mod:update_rating(SourceID, ItemID, NewRating, OldRatings, Data0),
    {reply, ok, State#state{opaque = Data1}};
handle_call(
    {predict_all, Ratings, Options},
    _From,
    State = #state{callback = Mod, opaque = Data0}
) ->
    Prediction = Mod:predict_all(Ratings, Data0, Options),
    {reply, Prediction, State};
handle_call(
    print_debug,
    _From,
    State = #state{callback = Mod, opaque = Data0}
) ->
    Mod:print_debug(Data0),
    {reply, ok, State};
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
%empty


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
