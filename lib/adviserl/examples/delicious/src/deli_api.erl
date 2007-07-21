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
%%% @doc delicious API over HTTP
%%%
%%% Using RSS avoid would authentication and enable to access any
%%% user content (but I think you cannot get all data).
%%% Ex: {ok, Data} = http:request("http://del.icio.us/rss/khigia"),
%%% Ex: {Doc,_}=xmerl_scan:string(Data),
%%% Ex: lists:map(
%%% Ex:     fun(#xmlAttribute{value=URL})-> URL end,
%%% Ex:     xmerl_xpath:string("/rdf:RDF/channel/items/rdf:Seq/rdf:li/@rdf:resource", Doc)
%%% Ex: ).
%%%
%%% @end
-module(deli_api).


% ~~ Declaration: API
-export([
    posts_update/2,
    posts_all/2
]).


% ~~ Declaration: Internal

-include("include/delicious.hrl").

-include_lib("xmerl/include/xmerl.hrl").


% ~~ Implementation: API

% (string(),string()) -> {ok, {{Y,M,D},{H,M,S}}}|{error,Reason}
posts_update(User, Password) ->
    case http:request(
        get,
        {
            "https://api.del.icio.us/v1/posts/update",
            %TODO need to set User-Agent
            [http_header_basic_authorization(User, Password)]
        },
        [{ssl, []}],
        []
    ) of
        {ok, {{_HTTPVersion, 200, _HTTPRespond}, _Headers, Body}} ->
            {ok, parse_api_update(Body)};
        Error ->
            Error
    end.

posts_all(User, Password) ->
    case http:request(
        get,
        {
            "https://api.del.icio.us/v1/posts/all?",
            %TODO need to set User-Agent
            [http_header_basic_authorization(User, Password)]
        },
        [{ssl, []}],
        []
    ) of
        {ok, {{_HTTPVersion, 200, _HTTPRespond}, _Headers, Body}} ->
            {ok, parse_api_all(Body)};
        Error ->
            Error
    end.


% ~~ Implementation: Internal


http_header_basic_authorization(User, Password) ->
    {
        "Authorization",
        "Basic " ++ http_base_64:encode(User ++ ":" ++ Password)
    }.

parse_api_update(Body) ->
    {Doc ,_} = xmerl_scan:string(Body),
    parse_api_update_xml(Doc).

parse_api_update_xml(Doc) ->
    [#xmlAttribute{value=DateTimeString}] = xmerl_xpath:string("/update/@time", Doc),
    from_iso8601(DateTimeString).

parse_api_all(Body) ->
    {Doc ,_} = xmerl_scan:string(Body),
    parse_api_all_xml(Doc).

parse_api_all_xml(Doc) ->
    [#xmlAttribute{value=DateTimeString}] = xmerl_xpath:string("/posts/@update", Doc),
    Timestamp = from_iso8601(DateTimeString),
    XMLPosts = xmerl_xpath:string("/posts/post", Doc),
    Posts = lists:map(
        fun(XMLPost) ->
            [#xmlAttribute{value=HRef}] = xmerl_xpath:string("@href", XMLPost),
            [#xmlAttribute{value=Hash}] = xmerl_xpath:string("@hash", XMLPost),
            [#xmlAttribute{value=Desc}] = xmerl_xpath:string("@description", XMLPost),
            [#xmlAttribute{value=TimeString}] = xmerl_xpath:string("@time", XMLPost),
            Time = from_iso8601(TimeString),
            [#xmlAttribute{value=Tag}] = xmerl_xpath:string("@tag", XMLPost),
            Tags = string:tokens(Tag, " "),
            #delipost{
                href=HRef,
                hash=Hash,
                description=Desc,
                tags=Tags,
                timestamp=Time
            }
        end,
        XMLPosts
    ),
    {Timestamp, Posts}.

% ex: from_iso8601("2007-07-19T00:16:33Z")
% assume ISO 8601 in UTC (http://www.w3.org/TR/NOTE-datetime)
from_iso8601(DateTimeString) ->
    case io_lib:fread("~u-~u-~uT~u:~u:~uZ", DateTimeString) of
        {ok, [Year,Month,Day,Hour,Minute,Second], []} ->
            {{Year,Month,Day},{Hour,Minute,Second}};
        Error ->
            Error
    end.

