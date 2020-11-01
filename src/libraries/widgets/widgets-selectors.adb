--  widgets-selectors.adb ---

--  Copyright 2020 cnngimenez
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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;
with Console;
use Console;

package body Widgets.Selectors is

    procedure Add (Selector : in out Selector_Type; Data : String) is
    begin
        Selector.Data.Append (To_Unbounded_String (Data));
    end Add;

    procedure Execute (Selector : in out Selector_Type) is
        Accepted : Boolean := False;
        Key : Character;
    begin
        Selector.Current_String := To_Unbounded_String ("");

        while not Accepted loop
            Selector.Put_Data;

            Put_Line (To_String (Selector.Current_String));

            Get_Immediate (Key);
            if Key = Character'Val (13) or else
              Key = Character'Val (10) then
                Accepted := True;
            else
                Append (Selector.Current_String, Key);
            end if;

        end loop;
    end Execute;

    function Filter_Data (Selector : Selector_Type;
                          Substring : Unbounded_String)
                         return Data_Vector is
    begin
        return Selector.Filter_Data (To_String (Substring));
    end Filter_Data;

    function Filter_Data (Selector : Selector_Type;
                          Substring : String)
                         return Data_Vector is
        Results : Data_Vector;
    begin
        if Substring = "" then
            Results := Selector.Data;
            return Results;
        end if;

        for A_String of Selector.Data loop
            if Index (A_String, Substring) > 0 then
                Results.Append (A_String);
            end if;
        end loop;

        return Results;
    end Filter_Data;

    function Get_Current_String (Selector : Selector_Type)
                      return Unbounded_String is
    begin
        return Selector.Current_String;
    end Get_Current_String;

    function Get_Current_String (Selector : Selector_Type)
                      return String is
    begin
        return To_String (Selector.Current_String);
    end Get_Current_String;

    function Get_Data (Selector : Selector_Type)
                      return Data_Vector is
    begin
        return Selector.Data;
    end Get_Data;

    procedure Put_Data (Selector : Selector_Type) is
        Filtered_Data : Data_Vector;
        A_String : Unbounded_String;
    begin
        Erase_Display (Entire_Screen);

        Filtered_Data := Selector.Filter_Data (Selector.Current_String);

        for I in 1 .. 10 loop
            A_String := To_Unbounded_String ("");
            if I < Integer (Filtered_Data.Length) then
                A_String := Filtered_Data (I);
            end if;

            Put_Line (To_String (A_String));
        end loop;
    end Put_Data;

    procedure Set_Current_String (Selector : in out Selector_Type;
                                  Current_String : Unbounded_String) is
    begin
        Selector.Current_String := Current_String;
    end Set_Current_String;

    procedure Set_Data (Selector : in out Selector_Type;
                        Data : Data_Vector) is
    begin
        Selector.Data := Data;
    end Set_Data;

end Widgets.Selectors;
