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
%%% @doc Example: recommend tags from delicious
%%%
%%% @end
-module(deli_keywords).

-export([start/0]).

-include("include/delicious.hrl").

start() ->
    application:start(ssl),
    application:start(inets),
    application:start(sasl),
    application:start(adviserl),
    {ok, DeliciousPID} = deli_posts:start_link(),
    {User, Password, Options} = prompt_delicious_authentication(),
    load_delicious_data(DeliciousPID, User, Password, Options),
    loop().

prompt_delicious_authentication() ->
    User = case io:get_line('Enter a delicious login: ') of 
        eof ->
            "";
        "\n" ->
            "";
        RawInput ->
            lists:nth(1, string:tokens(RawInput, " \n"))
    end,
    Options = case io:get_line('Do you want to connect to del.icio.us (may use cache if no connection) [y/n]? ') of 
        "y\n" ->
            [];
        _ ->
            [no_update]
    end,
    Password = case Options of
        [] ->
            File = "passwords",
            case file:consult(File) of
                {ok, Passwords} ->
                    case proplists:get_value(User, Passwords) of
                        P when is_list(P) ->
                            P;
                        _ ->
                            io:format("ERROR: no password found for '~s' in file '~s'; no connection to delicious (file should contain any number of tuple {\"user\",\"password\"} ended by a point)", [User, File]),
                            ""
                    end;
                _ ->
                    io:format("ERROR: file '~s' not found; no connection to delicious", [File]),
                    ""
            end;
        _ ->
            ""
    end,
    {User, Password, Options}.

prompt_keywords() ->
    case io:get_line('Enter few keywords (space separated; empty to quit): ') of 
        eof ->
            "";
        RawInput ->
            RawInput
    end.

load_delicious_data(DeliciousPID, User, Password, Options) ->
    gen_server:call(DeliciousPID, {login, User, Password}),
    {ok, Posts, _Status} = gen_server:call(DeliciousPID, {get_posts, User, Options}, infinity),
    io:format("Loading posts", []),
    lists:foreach(
        fun(#delipost{href=HRef,tags=Tags}) ->
            io:format(".", []),
            %io:format("~s => ~p~n", [HRef,Tags]),
            lists:foreach(
                fun(Tag) -> adviserl:rate(HRef,Tag,{1,no}) end,
                Tags
            )
        end,
        Posts
    ),
    io:format("~n", []).

loop() ->
    Keywords = string:tokens(prompt_keywords(), " \n"),
    io:format("keywords:~p~n", [Keywords]),
    case Keywords of
        [] ->
            io:format("Bye~n", []),
            erlang:halt();
        _ ->
            io:format("step 1~n", []),
            KeywordIDs = lists:map(fun(K) -> adv_items:id_from_key(K) end, Keywords),
            io:format("step 2~n", []),
            IDs = lists:filter(fun(undefined) -> false ; (_) -> true end, KeywordIDs),
            io:format("step 3~n", []),
            Ratings = lists:map(fun(ID) -> {ID, 1} end, IDs),
            io:format("step 4~n", []),
            Rec = adviserl:recommend_all(Ratings),
            N = 10,
            io:format("Top ~w recommendations: ~p~n", [N, lists:sublist(Rec,N)]),
            loop()
    end.

