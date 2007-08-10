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
%%% @doc Adviserl shared-API.
%%%
%%% @end


% Global declaration

-define(SOURCES_PNAME,     adv_sources).
-define(ITEMS_PNAME,       adv_items).
-define(RATINGS_PNAME,     adv_ratings).
-define(PREDICTIONS_PNAME, adv_predictions).


% Data structures

-record(advsrc, {
    id,
    key,
    data
}).

-record(advitm, {
    id,
    key,
    data
}).


% Trace utilities

-ifdef(LOG_DEBUG).

-define(DEBUG(Msg, Params),   adv_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(INFO(Msg, Params),    adv_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(WARNING(Msg, Params), adv_util:log(?MODULE, ?LINE, dbg, Msg, Params)).
-define(ERROR(Msg, Params, Exception), adv_util:log(?MODULE, ?LINE, dbg, Msg, Params), throw(Exception)).

-else.

-define(DEBUG(Msg, Params),   true).
-define(INFO(Msg, Params),    adv_util:log(?MODULE, ?LINE, inf, Msg, Params)).
-define(WARNING(Msg, Params), adv_util:log(?MODULE, ?LINE, wrn, Msg, Params)).
-define(ERROR(Msg, Params, Exception), adv_util:log(?MODULE, ?LINE, err, Msg, Params), throw(Exception)).

-endif.
