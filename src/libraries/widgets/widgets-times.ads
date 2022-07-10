--  widgets-times.ads ---

--  Copyright 2022 cnngimenez
--
--  Author: cnngimenez

--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/Licenses/>.

-------------------------------------------------------------------------

with Ada.Calendar;
use Ada.Calendar;
with Widgets.Labels;
use Widgets.Labels;

package Widgets.Times is
    type Time_Type is new Label_Type with private;

    procedure Initialize (Widget : in out Time_Type;
                          Text : Wide_Wide_String;
                          Row, Column : Natural);

    procedure Set_Time (Widget : in out Time_Type;
                        Datetime : Time);
    procedure Set_Current (Widget : in out Time_Type;
                           Current : Boolean);

    function Get_Time (Widget : Time_Type) return Time;
    function Get_Current (Widget : Time_Type) return boolean);

    overriding procedure Draw (Time : in out Time_Type);

private
    type Time_Type is new Label_Type with
    record
        Current : Boolean;
        Datetime : Time;
    end record;

end Widgets.Times;
