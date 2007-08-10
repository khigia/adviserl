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
%%% @doc Types used in Adviserl application modules (documentation module).
%%%
%%% @end
-module(adv_types).

%%% @type sourceID() = integer().
%%%     Identifier of rating source.

%%% @type itemID() = integer().
%%%     Identifier of rated item.

%%% @type ratingValue() = integer().
%%%     Effective value of a rating.

%%% @type ratingData() = term().
%%%     Any associated (external) data to a rating (eg, date of rating).

%%% @type rating() = {RatingValue, RatingData}
%%%     RatingValue = ratingValue()
%%%     RatingData = ratingData().
%%%     Rating associated from one source to one item.

%%% @type ratings() = term().
%%%     Abstract sequence of ratings.

%%% @type predictions() = [{itemID(), real()}]
