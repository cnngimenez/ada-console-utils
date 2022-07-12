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

with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Calendar;
use Ada.Calendar;
with Widgets.Labels;
use Widgets.Labels;

package Widgets.Times is
    type Time_Type is new Label_Type with private;

    procedure Initialize (Widget : in out Time_Type;
                          Row, Column : Natural);
    procedure Initialize (Widget : in out Time_Type;
                          Label : Wide_Wide_String;
                          Row, Column : Natural);
    procedure Initialize (Widget : in out Time_Type;
                          Label : Unbounded_Wide_Wide_String;
                          Row, Column : Natural);

    procedure Set_Label (Widget : in out Time_Type; Label : Wide_Wide_String);
    procedure Set_Label (Widget : in out Time_Type;
                         Label : Unbounded_Wide_Wide_String);

    procedure Set_Time (Widget : in out Time_Type; Datetime : Time);
    --  Set the date and Time.
    --
    --  If Use_Current is False, use this time to display on the widget.

    procedure Set_Use_Current (Widget : in out Time_Type; Current : Boolean);
    --  Use the current time or the Datetime provided by the User.
    --
    --  If Current is true, the widgets will use the system time and updates
    --  the time each time Draw is Called.

    procedure Set_Show_Date (Widget : in out Time_Type; Show : Boolean);
    procedure Set_Show_Time (Widget : in out Time_Type; Show : Boolean);

    function Get_Label (Widget : Time_Type) return Wide_Wide_String;
    function Get_Label (Widget : Time_Type) return Unbounded_Wide_Wide_String;
    function Get_Time (Widget : Time_Type) return Time;
    function Get_Use_Current (Widget : Time_Type) return Boolean;

    overriding procedure Draw (Widget : in out Time_Type);

private
    type Time_Type is new Label_Type with
    record
        Use_Current : Boolean;
        Show_Date : Boolean;
        Show_Time : Boolean;
        Datetime : Time;
        Label : Unbounded_Wide_Wide_String;
    end record;

    procedure Update_Time (Widget : in out Time_Type);
    --  Update the time in the widgets with the current system Time.

    procedure Update_Time_String (Widget : in out Time_Type);

    procedure Update_Size (Widget : in out Time_Type);
end Widgets.Times;
