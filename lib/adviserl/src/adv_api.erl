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
%%% @doc adviserl API through an OTP gen_server.
%%%
%%% The call respond to those messages:
%%% <ul>
%%%   <li>`{get_rating_list, SourceID} -> [{ItemID, RatingIntegerValue}]'</li>
%%%   <li>`{rate, SourceID, ItemID, RatingValue} -> ok'</li>
%%%   <li>`{rate, SourceID, ItemID, Updater, Default} -> ok'</li>
%%%   <li>`{recommend_all, SourceIDOrRatingList} -> predictions()'</li>
%%%   <li>`{recommend_all, SourceIDOrRatingList, Options} -> predictions()'</li>
%%% </ul>
%%%
%%% For more info, see adviserl documentation.
%%%
%%% @end
-module(adv_api).


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
    start_link/0
]).


% ~~ Declaration: Internal

-include("include/adviserl.hrl").


% ~~ Implementation: API

%%% @doc  Start server locally registered.
%%% @see  gen_server:start_link/4
%%% @end
start_link() ->
    gen_server:start_link({local, ?API_PNAME}, ?MODULE, [], []).


% ~~ Implementation: Behaviour callbacks

init(_InitArgs) ->
    {ok, nostate}.

handle_call({get_rating_list, SourceID}, _From, State) ->
    case adv_ratings:get_ratings(SourceID) of
        undefined ->
            {reply, [], State};
        Ratings ->
            {reply, adv_ratings:to_list(Ratings), State}
    end;

handle_call({rate, SourceID, ItemID, Rating}, _From, State) ->
    {reply, adviserl:rate(SourceID, ItemID, Rating), State};

handle_call({rate, SourceID, ItemID, Updater, Default}, _From, State) ->
    {reply, adviserl:rate(SourceID, ItemID, Updater, Default), State};

handle_call({recommend_all, SourceIDOrKeyOrRatings}, _From, State) ->
    {reply, adviserl:recommend_all(SourceIDOrKeyOrRatings), State};

handle_call({recommend_all, SourceIDOrKeyOrRatings, Options}, _From, State) ->
    {reply, adviserl:recommend_all(SourceIDOrKeyOrRatings, Options), State};

handle_call(_Request, _From, State) ->
    {reply, not_handled_call, State}.

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
