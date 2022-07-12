--  widgets-countdowns.adb ---

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

--  with Ada.Text_IO;
--  use Ada.Text_IO;
with Ada.Calendar.Arithmetic;
use Ada.Calendar.Arithmetic;
--  with Ada.Calendar.Delays;
--  use Ada.Calendar.Delays;

package body Widgets.Countdowns is

    overriding procedure Draw (Widget : in out Countdown_Type) is
    begin
        Widget.Update_Difference;
        Widgets.Labels.Draw (Label_Type (Widget));
    end Draw;

    function Get_Label (Widget : Countdown_Type)
             return Unbounded_Wide_Wide_String
    is
    begin
        return Widget.Label;
    end Get_Label;

    function Get_Label (Widget : Countdown_Type) return Wide_Wide_String is
    begin
        return To_Wide_Wide_String (Widget.Label);
    end Get_Label;

    function Get_To_Time (Widget : Countdown_Type) return Time is
    begin
        return Widget.To_Time;
    end Get_To_Time;

    procedure Initialize (Widget : in out Countdown_Type;
                          To_Time : Time;
                          Row, Column : Natural) is
    begin
        --  Width := "+0000 00:00:00"'Length
        Widgets.Labels.Initialize (Label_Type (Widget), "+0000 00:00:00",
                                   Row, Column, 1, 14);
        Widget.To_Time := To_Time;
    end Initialize;

    procedure Initialize (Widget : in out Countdown_Type;
                          Label : Unbounded_Wide_Wide_String;
                          To_Time : Time;
                          Row, Column : Natural) is
    begin
        Widget.Initialize (To_Time, Row, Column);
        Widget.Label := Label;
    end Initialize;

    procedure Initialize (Widget : in out Countdown_Type;
                          Label : Wide_Wide_String;
                          To_Time : Time;
                          Row, Column : Natural) is
    begin
        Widget.Initialize (To_Time, Row, Column);
        Widget.Label := To_Unbounded_Wide_Wide_String (Label);
    end Initialize;

    procedure Set_Label (Widget : in out Countdown_Type;
                         Label : Unbounded_Wide_Wide_String) is
    begin
        Widget.Label := Label;
    end Set_Label;

    procedure Set_Label (Widget : in out Countdown_Type;
                         Label : Wide_Wide_String) is
    begin
        Widget.Label := To_Unbounded_Wide_Wide_String (Label);
    end Set_Label;

    procedure Set_To_Time (Widget : in out Countdown_Type; To_Time : Time) is
    begin
        Widget.To_Time := To_Time;
    end Set_To_Time;

    procedure Update_Difference (Widget : in out Countdown_Type) is
        Now : constant Ada.Calendar.Time := Clock;
        Negative_Str : Wide_Wide_String := " ";
        Days : Day_Count;
        Seconds : Duration;
        Leap_Seconds : Leap_Seconds_Count;
        Temp, Hours, Minutes, Toseconds : Natural;
    begin
        Difference (Widget.To_Time, Now, Days, Seconds, Leap_Seconds);

        --  Seconds has the remainding seconds within a Day.
        --  Therefore, -86400 .. 86400 which is in the Integer range.
        Temp := Natural (abs (Seconds));
        Hours :=  Temp / 60 / 60;
        Minutes := Temp / 60 - Hours * 60;
        Toseconds := Temp mod 60;

        --  --  Debugging purposes only:
        --  Put_Line (Days'Image  & "d - "
        --      & Hours'Image & "h - "
        --      & Minutes'Image & "m - "
        --      & Toseconds'Image & "S");

        if Seconds < Duration (0) then
            Negative_Str := "-";
        end if;

        Widgets.Labels.Set_Text (Label_Type (Widget),
                                 To_Wide_Wide_String (Widget.Label) & " "
                                 & Negative_Str
                                 & Days'Wide_Wide_Image & " "
                                 & Hours'Wide_Wide_Image & ":"
                                 & Minutes'Wide_Wide_Image & ":"
                                 & Toseconds'Wide_Wide_Image);
    end Update_Difference;

end Widgets.Countdowns;
