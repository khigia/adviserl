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
%%% @doc delicious-like posts server.
%%%
%%% @end
-module(deli_posts).


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

-record(st, {
    user = undefined,
    password_fun = undefined % () -> string() % avoid log of password
    %use a DETS table delicious_posts: {user, timestamp, posts} % TODO maybe a DB like table with multiple rows {user,timestamp,post}
}).



% ~~ Implementation: API

%%% @doc  Start server localy registered.
%%% @see  gen_server:start_link/4
%%% @end
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


% ~~ Implementation: Behaviour callbacks

init(_InitArgs) ->
    open_posts(),
    State = #st{
    },
    {ok, State}.

handle_call({login, User, Password}, _From, State) 
    when
        is_list(User),
        is_list(Password)
    ->
    {reply, ok, State#st{user=User, password_fun=fun()-> Password end}};
handle_call(logout, _From, State) ->
    {reply, ok, State#st{user=undefined, password_fun=undefined}};
handle_call({get_posts, User, Options}, _From, State=#st{user=User})
    when
        is_list(User)
    ->
    %strategy 1: call api update; if not uptodate, call api all
    PasswordFun = State#st.password_fun,
    Password = PasswordFun(),
    case get_posts(User) of
        {User, CurTimestamp, CurPosts} ->
            case deli_api:update(User, Password) of
                {ok, Timestamp} ->
                    case CurTimestamp < Timestamp of
                        true ->
                            case deli_api:posts_all(User, Password) of
                                {ok, {PTimestamp, Posts}} ->
                                    put_posts(
                                        User,
                                        PTimestamp,
                                        Posts
                                    ),
                                    {reply, {ok, Posts, uptodate}, State};
                                _ ->
                                    {reply, {ok, CurPosts, archive}, State}
                            end;
                        _ ->
                            {reply, {ok, CurPosts, uptodate}, State}
                    end;
                _Error ->
                    {reply, {ok, CurPosts, archive}, State}
            end;
        empty ->
            case deli_api:posts_all(User, Password) of
                {ok, {RTimestamp, Posts}} ->
                    put_posts(
                        User,
                        RTimestamp,
                        Posts
                    ),
                    {reply, {ok, Posts, uptodate}, State};
                _ ->
                    {reply, {ok, [], archive}, State}
            end;
        Error ->
            {reply, Error, State}
    end;
    %TODO strategy 2: call api update; if not uptodate, call api date then get for each
handle_call({get_posts, User, Options}, _From, State=#st{user=AuthUser})
    when
        is_list(User),
        User /= AuthUser
    ->
    % TODO we may support this feature using rss api
    case get_posts(User) of
        {User, _CurTimestamp, CurPosts} ->
            {reply, {ok, CurPosts, archive}, State};
        empty ->
            {reply, {ok, [], archive}, State};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, unknown_request, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    close_posts(),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% ~~ Implementation: Internal

open_posts() ->
    {ok, delicious_posts} = dets:open_file(delicious_posts, [
        {file, "delicious_posts.dets"},
        {auto_save, 30000},
        %{ram_file, true},
        {type, set}
    ]).

close_posts() ->
    dets:close(delicious_posts).

get_posts(User) ->
    case dets:lookup(delicious_posts, User) of
        [] ->
            empty;
        [Data] ->
            Data;
        _ ->
            {error, "Internal error: more than one entry in cache"}
    end.

put_posts(User, Timestamp, Posts) ->
    dets:insert(delicious_posts, {User, Timestamp, Posts}).


% ~~ Unit tests
-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").


-endif.
