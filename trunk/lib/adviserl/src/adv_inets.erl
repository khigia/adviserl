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
%%% @doc adviserl API as inets HTTP server (using ESI callbacks).
%%% @end
-module(adv_inets).

-export([
    rate_id/3,
    recommend_all/3
]).


% HTTP/ESI helpers

%% @spec header(Status::atom(), [Field]) -> string()
%%     Status = ok|bad_request|not_found
%%     Field = {Name::string(), Value::string()}
%% @doc Generate a complete HTTP header.
%% @end
% Inets generate the status line from the 'status' header field.
header(ok, Fields) ->
    header([{"Status","200 OK"} | Fields]);
header(bad_request, Fields) ->
    header([{"Status","400 Bad request"} | Fields]);
header(not_found, Fields) ->
    header([{"Status","404 Not found"} | Fields]).

%% @spec header([{Name::string(),Value::string()}]) -> string()
%% @doc Generate all HTTP header fields and append header end delimiter.
header(Fields) ->
    lists:flatten([header_fields(Fields, []), "\r\n"]).

%% @spec header_fields([{Name,Value}], Acc) -> DeepList
%% @doc Generate HTTP header fields
header_fields([], Acc) ->
    Acc;
header_fields([{FieldName,FieldValue} | Fields], Acc) ->
    header_fields(Fields, [[FieldName, ":", FieldValue, "\r\n"] | Acc]).

%% @spec query_terms(HTTPQuery, Parameters, Options) -> Result | {error,Reason}
%%     Parameters = [string()]
%%     Options    = [string()]
%%     Result     = {ok, [Mandatory::term()], [Optional::{Name::string(), Value::term()}]}
%% @doc Parse query to extract mandatory and optional parameters.
query_terms(Query, Mandatory, Optional) ->
    Options = lists:foldl(
        fun(Name, Acc) ->
            case proplists:lookup(Name, Query) of
                none ->
                    Acc;
                {Name, Repr} ->
                    case query_term(Repr) of
                        {ok, Value} ->
                            [{Name, Value} | Acc];
                        _ ->
                            Acc
                    end
            end
        end,
        [],
        Optional
    ),
    Parameters = lists:foldl(
        fun
            (_Param, Err = {error, _Reason}) ->
                Err;
            (Param, Acc) ->
                case proplists:get_value(Param, Query) of
                    undefined ->
                        {error, "missing parameter"};
                    Repr ->
                        case query_term(Repr) of
                        {ok, Value} ->
                            [Value | Acc];
                        _ ->
                            {error, "invalid parameter"}
                    end
                end
        end,
        [],
        Mandatory
    ),
    case Parameters of
        Err = {error, _Reason} ->
            Err;
        _ ->
            {ok, lists:reverse(Parameters), Options}
    end.

%% @spec query_term(Repr::string()) -> {ok, term()} | {error, Reason}
%% @doc Extract erlang value of string representation
query_term(Repr) ->
    case (catch query_term_value(Repr)) of
        {ok, Value} ->
            {ok, Value};
        _ ->
            {error, "invalid input"}
    end.

%% @spec query_term_value(Repr::string()) -> term()
%% @doc Convert from string to erlang value (may fail).
query_term_value(Repr) ->
    {ok, TermTokens, _EndLine} = erl_scan:string(Repr ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(TermTokens),
    {value, Value, _NewBindings} = erl_eval:exprs(Exprs, []),
    {ok, Value}.


% HTML generation helpers

rev(L) ->
    lists:reverse(lists:flatten(L)).

tag(Tag, Content) ->
    tag(Tag, [], Content).

tag(Tag, Attributes, Content) ->
    ["<", Tag, tag_attributes(Attributes, []), ">", Content, "</", Tag, ">"].

tag_attributes([], Acc) ->
    Acc;
tag_attributes([{Name, Value} | Attributes], Acc) ->
    tag_attributes(Attributes, [[" ", Name, "=\"", Value, "\""] | Acc]).

body(Content) ->
    tag("body", Content).

html(Content) ->
    tag("html", Content).

head(Title, Content) ->
    tag("head", [tag("title", Title) | Content]).

h1(Content) ->
    tag("h1", Content).

h2(Content) ->
    tag("h2", Content).

form(Action, Method, Fieldsets, Inputs) ->
    tag("form", [
        {"method", Method},
        {"action", Action}
    ], [
        form_fieldsets(Fieldsets, []),
        form_inputs(Inputs, [])
    ]).

form_fieldsets([], Acc) ->
    rev(Acc);
form_fieldsets([{Legend, Fields} | Fieldsets], Acc) ->
    form_fieldsets(Fieldsets, [ rev(tag("fieldset", [
        tag("legend", Legend),
        form_fieldset_fields(Fields, [])
    ])) | Acc]).

form_fieldset_fields([], Acc) ->
    rev(Acc);
form_fieldset_fields([{"text", ID, Name, Label} | Fields], Acc) ->
    form_fieldset_fields(
        Fields,
        [rev(tag("dl", [
            tag("dt", tag("label", [{"for", ID}], Label)),
            tag("dd", tag("input", [{"type", "text"}, {"id", ID}, {"name", Name}], []))
        ])) | Acc]
    );
form_fieldset_fields([{"submit", ID, Name, Label} | Fields], Acc) ->
    form_fieldset_fields(Fields, [rev(
        tag("dl", [
            tag("dt", tag("label", [{"for", ID}], Label)),
            tag("dd", tag("input", [{"type", "text"}, {"id", ID}, {"name", Name}], []))
        ])
    )| Acc]).

form_inputs([], Acc) ->
    rev(Acc);
form_inputs([{Type, Name, Value} | Inputs], Acc) ->
    form_inputs(Inputs, [rev(
        tag("input", [
            {"name", Name},
            {"type", Type},
            {"value", Value}
        ], [])
    )| Acc]).


% ESI callbacks

%% @doc rate_id API accepting GET or POST methods.
%% The URL query must define:
%% <ul>
%%     <li>`s' as the SourceID</li>
%%     <li>`i' as the ItemID</li>
%%     <li>`r' as the Rating score</li>
%% </ul>
%% Optionaly the query may define:
%% <ul>
%%     <li>`d' as any rating Data</li>
%% </ul>
%% The HTTP return code is:
%% <ul>
%%     <li>400 (bad request) if parameters are not set correctly
%%         (header field Reason may give hint on failure cause).</li>
%%     <li>404 (not found) if rating cannot be applied.</li>
%%     <li>200 (ok) otherwise.</li>
%% </ul>
%% The HTTP body is empty for POST request and contains HTML for GET one.
rate_id(SessionID, Env, Input) ->
    Query = httpd:parse_query(Input),
    case query_terms(Query, ["s", "i", "r"], ["d"]) of
        {error, Reason} ->
            mod_esi:deliver(SessionID, header(bad_request, [
                {"Reason", Reason}
            ]));
        {ok, [SourceID, ItemID, Rating], Options} ->
            case proplists:get_value(request_method, Env) of
                "POST" ->
                    Result = process_rate_id(SourceID, ItemID, Rating, Options),
                    case Result of
                        ok ->
                            mod_esi:deliver(SessionID, header(ok, []));
                        _ ->
                            mod_esi:deliver(SessionID, header(not_found, []))
                    end;
                _ ->
                    Result = process_rate_id(SourceID, ItemID, Rating, Options),
                    respond_rate_id(SessionID, Env, {Result, SourceID, ItemID, Rating, Options})
            end
    end.

%% @doc recommend_all API accepting GET or POST methods.
%% The URL query must define:
%% <ul>
%%     <li>`s' as the SourceID</li>
%% </ul>
%% Optionaly the query may define:
%% <ul>
%%     <li>`no_key_lookup=1'</li>
%%     <li>`no_remove_known=1'</li>
%% </ul>
%% The HTTP return code is:
%% <ul>
%%     <li>400 (bad request) if parameters are not set correctly
%%         (header field Reason may give hint on failure cause).</li>
%%     <li>404 (not found) if recommendation cannot be retrieve (bad source).</li>
%%     <li>200 (ok) otherwise, with plain text body: each line specify
%%         one item and score (space separated).</li>
%% </ul>
%% The HTTP body is empty for POST request and contains HTML for GET one.
recommend_all(SessionID, Env, Input) ->
    Query = httpd:parse_query(Input),
    case query_terms(Query, ["s"], ["no_key_lookup", "no_remove_known"]) of
        {error, Reason} ->
            mod_esi:deliver(SessionID, header(bad_request, [
                {"Reason", Reason}
            ]));
        {ok, [Source], Options} ->
            respond_recommend_all(SessionID, Env, Source, Options)
    end.

process_rate_id(SourceID, ItemID, Rating, Options) ->
    RatingData = case proplists:get_value("d", Options) of
        undefined ->
            no_data;
        Value ->
            Value
    end,
    (catch adviserl:rate_id(SourceID, ItemID, {Rating, RatingData})).

process_recommend_all(Source, Options) ->
    AdvOptions0 = [],
    AdvOptions1 = case proplists:get_value("no_key_lookup", Options) of
        1 ->
            [{no_key_lookup, true} | AdvOptions0];
        _ ->
            AdvOptions0
    end,
    AdvOptions2 = case proplists:get_value("no_remove_known", Options) of
        1 ->
            [{no_remove_known, true} | AdvOptions1];
        _ ->
            AdvOptions1
    end,
    (catch adviserl:recommend_all(Source, AdvOptions2)).

respond_rate_id(SessionID, Env, {Result, SourceID, ItemID, Rating, Options}) ->
    Respond = lists:flatten(
        html([
            head("rate_id result", []),
            body([
                h1("Rating request result"),
                h2("Env"),
                io_lib:format("Env:~p~n", [Env]),
                h2("Query"),
                io_lib:format("SourceID: ~p<br/>~n", [SourceID]),
                io_lib:format("ItemID: ~p<br/>~n", [ItemID]),
                io_lib:format("Rating: ~p<br/>~n", [Rating]),
                io_lib:format("Options: ~p<br/>~n", [Options]),
                h2("Result"),
                io_lib:format("~p<br/>~n", [Result]),
                h1("New rating request"),
                form(
                    "/api/adv_inets:rate_id",
                    "GET",
                    [
                        {"Mandatory", [
                            {"text", "sourceid", "s", "SourceID"},
                            {"text", "itemid",   "i", "ItemID"},
                            {"text", "rating",   "r", "Rating"}
                        ]},
                        {"Optional", [
                            {"text", "data",     "d", "Rating data"}
                        ]}
                    ],
                    [
                        {"submit", "commit", "Rate"}
                    ]
                )
            ])
         ])
     ),
    mod_esi:deliver(SessionID, lists:flatten(
        header(ok, [
            {"Content-Type", "text/html"},
            {"Content-Length", integer_to_list(length(Respond))},
            {"Date",         httpd_util:rfc1123_date()}
        ]),
        Respond
    )).

respond_recommend_all(SessionID, Env, Source, Options) ->
    case process_recommend_all(Source, Options) of
        Predictions when is_list(Predictions) ->
            Respond = prediction_to_string(Predictions),
            mod_esi:deliver(SessionID, lists:flatten(
                header(ok, [
                    {"Content-Type", "text/plain"},
                    {"Content-Length", integer_to_list(length(Respond))},
                    {"Date",         httpd_util:rfc1123_date()}
                ]),
                Respond
            ));
        _ ->
            % adviserl error, or adviserl not reached
            mod_esi:deliver(SessionID, header(not_found, []))
    end.

prediction_to_string(Predictions) ->
    lists:flatmap(
        fun({Item, Score}) -> io_lib:format("~w ~w~n", [Item, Score]) end,
        Predictions
    ).
