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

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Console;
use Console;

package body Widgets.Selectors is

    procedure Add (Selector : in out Selector_Type;
                   Data : Wide_Wide_String) is
        use Data_Vectors_Sorting;
    begin
        Selector.Data.Append (To_Unbounded_Wide_Wide_String (Data));
        Sort (Selector.Data);
    end Add;

    procedure Ask_If_New (Selector : in out Selector_Type) is
        use Data_Vectors;
        Possible_Selection : Unbounded_Wide_Wide_String :=
          To_Unbounded_Wide_Wide_String ("");
        Key : Wide_Wide_Character;

    begin
        if Selector.Current_String = "" then
            --  No text written.
            Selector.Current_String := Element (Selector.Data,
                                                Selector.Current_Selection);
            return;
        end if;

        --  Check if the current string is a substring of the current selection
        --  Use the filtered selections which is the one showed to the user.
        Possible_Selection := Element (Filter_Data (Selector,
                                                    Selector.Current_String),
                                       Selector.Current_Selection);

        if Index (Possible_Selection,
                  To_Wide_Wide_String (Selector.Current_String)) = 0
        then
            --  It is not, the delete the current selection.
            Possible_Selection := To_Unbounded_Wide_Wide_String ("");
        end if;

        if Possible_Selection = Selector.Current_String then
            --  Written text is the same as the selection
            return;
        end if;

        if Possible_Selection /= "" then
            Put_Line ("Current selection: " &
                        To_Wide_Wide_String (Possible_Selection));
            Put_Line ("Is the text '"
                        & To_Wide_Wide_String (Selector.Current_String)
                        & "' a new selected data?(Y/n)");
            Put_Line ("If answer is (n), " &
                        "then the current selection is returned.");
        else
            Put_Line ("Is the text '"
                        & To_Wide_Wide_String (Selector.Current_String)
                        & "' a new selected data?(Y/n)");
            Put_Line ("If answer is (n), then the empty string is returned.");
        end if;

        loop
            Get_Immediate (Key);
            if Key = 'y' or else Key = 'Y' then
                Put_Line (Key'Wide_Wide_Image);
                exit;
            elsif Key = 'n' or else Key = 'N' then
                Selector.Current_String := Possible_Selection;
                Put_Line (Key'Wide_Wide_Image);
                exit;
            end if;
        end loop;
        return;
    end Ask_If_New;

    procedure Execute (Selector : in out Selector_Type) is
        Accepted : Boolean := False;
        Key : Wide_Wide_Character;

        procedure Get_Escape_Sequence;

        procedure Get_Escape_Sequence is
            Key1, Key2 : Wide_Wide_Character;
        begin
            Get_Immediate (Key1);
            Get_Immediate (Key2);

            if Key2 = Wide_Wide_Character'Val (66) then
                Selector.Next_Selection;
            elsif Key2 = Wide_Wide_Character'Val (65) then
                Selector.Previous_Selection;
            end if;
        end Get_Escape_Sequence;

    begin
        Selector.Current_Selection := 1;
        Selector.Current_String := To_Unbounded_Wide_Wide_String ("");

        while not Accepted loop
            Erase_Display (Entire_Screen);
            Selector.Put_Data;

            Put_Line (To_Wide_Wide_String (Selector.Current_String));

            --  Put_Line (Positive'Image (Wide_Wide_Character'Pos (Key)));
            Get_Immediate (Key);

            if Key = Wide_Wide_Character'Val (13) or else
              Key = Wide_Wide_Character'Val (10)
            then
                Ask_If_New (Selector);
                Accepted := True;
            elsif Key = Wide_Wide_Character'Val (27) then
                Get_Escape_Sequence;
            else
                Append (Selector.Current_String, Key);
            end if;
        end loop;
    end Execute;

    function Filter_Data (Selector : Selector_Type;
                          Substring : Unbounded_Wide_Wide_String)
                         return Data_Vector is
    begin
        return Selector.Filter_Data (To_Wide_Wide_String (Substring));
    end Filter_Data;

    function Filter_Data (Selector : Selector_Type;
                          Substring : Wide_Wide_String)
                         return Data_Vector is
        use Data_Vectors;
        procedure Append_If_Has_Substring (Position : Cursor);

        Results : Data_Vector;

        procedure Append_If_Has_Substring (Position : Cursor) is
        begin
            if Index (Element (Position), Substring) > 0 then
                Results.Append (Element (Position));
            end if;
        end Append_If_Has_Substring;

    begin
        if Substring = "" then
            Results := Selector.Data;
            return Results;
        end if;

        Selector.Data.Iterate (Append_If_Has_Substring'Access);

        return Results;
    end Filter_Data;

    function Get_Current_String (Selector : Selector_Type)
                      return Unbounded_Wide_Wide_String is
    begin
        return Selector.Current_String;
    end Get_Current_String;

    function Get_Current_String (Selector : Selector_Type)
                                return Wide_Wide_String is
    begin
        return To_Wide_Wide_String (Selector.Current_String);
    end Get_Current_String;

    function Get_Data (Selector : Selector_Type)
                      return Data_Vector is
    begin
        return Selector.Data;
    end Get_Data;

    procedure Next_Selection (Selector : in out Selector_Type) is
    begin
        if Selector.Current_Selection < 10 then
            Selector.Current_Selection := Selector.Current_Selection + 1;
        else
            Selector.Current_Selection := 0;
        end if;
    end Next_Selection;

    procedure Previous_Selection (Selector : in out Selector_Type) is
    begin
        if Selector.Current_Selection > 0 then
            Selector.Current_Selection := Selector.Current_Selection - 1;
        else
            Selector.Current_Selection := 10;
        end if;
    end Previous_Selection;

    procedure Put_Data (Selector : Selector_Type) is
        Filtered_Data : Data_Vector;
        A_String : Unbounded_Wide_Wide_String;
    begin
        Filtered_Data := Selector.Filter_Data (Selector.Current_String);

        for I in 1 .. 10 loop
            A_String := To_Unbounded_Wide_Wide_String ("");
            if I <= Integer (Filtered_Data.Length) then
                A_String := Filtered_Data (I);
            end if;

            if I = Selector.Current_Selection then
                Set_Background (White);
                Set_Colour (Black);
                Blink;
            else
                Default_Background;
                Default_Colour;
                Blink_Off;
            end if;

            Put_Line (To_Wide_Wide_String (A_String));
        end loop;
    end Put_Data;

    procedure Set_Current_String (Selector : in out Selector_Type;
                                  Current_String : Unbounded_Wide_Wide_String)
    is
    begin
        Selector.Current_String := Current_String;
    end Set_Current_String;

    procedure Set_Data (Selector : in out Selector_Type;
                        Data : Data_Vector) is
        use Data_Vectors_Sorting;
    begin
        Selector.Data := Data;
        Sort (Selector.Data);
    end Set_Data;

end Widgets.Selectors;
