--  widgets-times.adb ---

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

with Ada.Calendar.Formatting;
with Ada.Strings.Wide_Wide_Unbounded;

package body Widgets.Times is

    overriding procedure Draw (Widget : in out Time_Type) is
    begin
        Widget.Update_Time;
        Widget.Update_Time_String;
        Widget.Update_Size;
        Widgets.Labels.Draw (Label_Type (Widget));
    end Draw;

    function Get_Time (Widget : Time_Type) return Time is
    begin
        return Widget.Datetime;
    end Get_Time;

    function Get_Use_Current (Widget : Time_Type) return Boolean is
    begin
        return Widget.Use_Current;
    end Get_Use_Current;

    procedure Initialize (Widget : in out Time_Type;
                          Row, Column : Natural) is
    begin
        Widgets.Labels.Initialize (Label_Type (Widget), "",
                                   Row, Column, 1, 18);

        Widget.Use_Current := True;
        Widget.Show_Date := True;
        Widget.Show_Time := True;

        --  Updates are done by Draw procedure.
    end Initialize;

    procedure Set_Show_Date (Widget : in out Time_Type; Show : Boolean) is
    begin
        Widget.Show_Date := Show;
    end Set_Show_Date;

    procedure Set_Show_Time (Widget : in out Time_Type; Show : Boolean) is
    begin
        Widget.Show_Time := Show;
    end Set_Show_Time;

    procedure Set_Time (Widget : in out Time_Type; Datetime : Time) is
    begin
        Widget.Datetime := Datetime;
        Widget.Use_Current := False;
    end Set_Time;

    procedure Set_Use_Current (Widget : in out Time_Type; Current : Boolean) is
    begin
        Widget.Use_Current := Current;
    end Set_Use_Current;

    procedure Update_Size (Widget : in out Time_Type) is
        Size : Natural := 0;
    begin
        --  Height is 8 because the length of "24:00:00".
        Size := Size + (if Widget.Show_Date then 8 else 0);
        --  Height is 10 because the length of "1970/01/01".
        Size := Size + (if Widget.Show_Time then 10 else 0);
        Widgets.Set_Width (Widget_Type (Widget), Size);
    end Update_Size;

    procedure Update_Time (Widget : in out Time_Type) is
    begin
        if Widget.Use_Current then
            Widget.Datetime := Clock;
        end if;
    end Update_Time;

    procedure Update_Time_String (Widget : in out Time_Type) is
        use Ada.Calendar.Formatting;
        use Ada.Strings.Wide_Wide_Unbounded;

        Time1 : constant Time := Widget.Datetime;
        Hour1 : Hour_Number;
        Minute1 : Minute_Number;
        Second1 : Second_Number;
        Day1 : Day_Number;
        Month1 : Month_Number;
        Year1 : Year_Number;

        Time_Str : Unbounded_Wide_Wide_String;
    begin
        if Widget.Show_Date then
            Day1 := Day (Time1, 0);
            Month1 := Month (Time1, 0);
            Year1 := Year (Time1, 0);
            Append (Time_Str, Day1'Wide_Wide_Image & "/"
                              & Month1'Wide_Wide_Image & "/"
                              & Year1'Wide_Wide_Image);
        end if;

        if Widget.Show_Time then
            Hour1 := Hour (Time1, 0);
            Minute1 := Minute (Time1, 0);
            Second1 := Second (Time1);
            Append (Time_Str, Hour1'Wide_Wide_Image & ":"
                              & Minute1'Wide_Wide_Image & ":"
                              & Second1'Wide_Wide_Image);
        end if;

        Widgets.Labels.Set_Text (Label_Type (Widget), Time_Str);
    end Update_Time_String;
end Widgets.Times;
