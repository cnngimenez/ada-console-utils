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
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions;
with Ada.Wide_Wide_Characters.Handling;
use Ada.Wide_Wide_Characters.Handling;
with Ada.Strings.Wide_Wide_Fixed;
use Ada.Strings.Wide_Wide_Fixed;

with Console.CSI_Codes;
use Console.CSI_Codes;
with Console.SGR;

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
        Filtered_Data : Data_Vector;

    begin
        if Selector.Current_String = "" then
            --  No text written.
            Selector.Current_String := Element (Selector.Data,
                                                Selector.Current_Selection);
            return;
        end if;

        --  Check if the current string is a substring of the current selection
        --  Use the filtered selections which is the one showed to the user.
        Filtered_Data := Filter_Data (Selector, Selector.Current_String);
        if Is_Empty (Filtered_Data) then
            Possible_Selection := To_Unbounded_Wide_Wide_String ("");
        else
            Possible_Selection := Element (Filtered_Data,
                                           Selector.Current_Selection);
        end if;

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

    function Current_Filter_Data (Selector : Selector_Type)
                                 return Data_Vector is
      (Filter_Data (Selector, Selector.Current_String));

    procedure Delete_Character (Selector : in out Selector_Type) is
        Amount : constant Natural := Length (Selector.Current_String);
    begin
        if Amount > 0 then
            Delete (Selector.Current_String,
                    Length (Selector.Current_String),
                    Length (Selector.Current_String));
        end if;
    end Delete_Character;

    overriding procedure Draw (Selector : in out Selector_Type) is

    begin
        Widgets.Draw (Widget_Type (Selector));
        Selector.Update_Filtered_Data;
        Selector.Put_Data;

        Console.SGR.Set_RGB_Background (100, 200, 100);
        Console.SGR.Set_Colour (Console.SGR.Black);
        Put_Line ("⌨ " & To_Wide_Wide_String (Selector.Current_String));
        Put_Line ("Selection number "
            & To_Wide_Wide_String (Selector.Current_Selection'Image)
            & " of "
            & To_Wide_Wide_String
                (Data_Vectors.Length (Selector.Filtered_Data)'Image)
            & " (total: "
            & To_Wide_Wide_String (Data_Vectors.Length (Selector.Data)'Image)
            & ").");
        Console.SGR.Reset_All;
    end Draw;

    procedure Execute (Selector : in out Selector_Type) is
        Accepted : Boolean := False;
        Key : Character;

        procedure Get_Escape_Sequence;

        procedure Get_Escape_Sequence is
            Key1, Key2 : Character;
        begin
            Ada.Text_IO.Get_Immediate (Key1);
            Ada.Text_IO.Get_Immediate (Key2);

            if Key2 = Character'Val (66) then
                Selector.Next_Selection;
            elsif Key2 = Character'Val (65) then
                Selector.Previous_Selection;
            end if;
        end Get_Escape_Sequence;

    begin
        Selector.Current_Selection := 1;
        Selector.Current_String := To_Unbounded_Wide_Wide_String ("");

        while not Accepted loop
            Console.CSI_Codes.Erase_Display (Console.CSI_Codes.Entire_Screen);
            Selector.Draw;
            --  Selector.Put_Data;

            --  Put_Line (To_Wide_Wide_String (Selector.Current_String));

            --  Put_Line (Positive'Image (Wide_Wide_Character'Pos (Key)));
            Ada.Text_IO.Get_Immediate (Key);

            if Key = Character'Val (13) or else
              Key = Character'Val (10)
            then
                --  Enter pressed
                Ask_If_New (Selector);
                Accepted := True;
            elsif Key = Character'Val (127) then
                Delete_Character (Selector);
            elsif Key = Character'Val (27) then
                Get_Escape_Sequence;
            else
                Append (Selector.Current_String, To_Wide_Wide_Character (Key));
                Selector.Current_Selection := 1;
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
            if Index (To_Lower (To_Wide_Wide_String (Element (Position))),
                      To_Lower (Substring)) > 0
            then
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

    function Get_Selected_String (Selector : Selector_Type)
                                 return Wide_Wide_String is
      (To_Wide_Wide_String (Get_Selected_String (Selector)));

    function Get_Selected_String (Selector : Selector_Type)
                                 return Unbounded_Wide_Wide_String is
        use Data_Vectors;

        Current_Data : constant Data_Vector := Selector.Current_Filter_Data;
    begin
        if Selector.Current_Selection > Natural (Length (Current_Data)) then
            return To_Unbounded_Wide_Wide_String ("");
        else
            return Current_Data (Selector.Current_Selection);
        end if;
    end Get_Selected_String;

    procedure Initialize (Selector : in out Selector_Type;
                          Row, Column : Natural) is
    begin
        Widgets.Initialize (Widget_Type (Selector),
                            Row, Column,
                            Default_Width, Default_Height);
        Selector.Current_Selection := 1;
        Selector.Current_String := To_Unbounded_Wide_Wide_String ("");
    end Initialize;

    overriding procedure Key_Event (Selector : in out Selector_Type;
                                    Key : Character)
    is
        Up_Key : constant Character := 'A';
        Down_Key : constant Character := 'B';
        Escape_Key : constant Character := Character'Val (27);
        Enter_Key : constant Character := Character'Val (13);
        Ret_Key : constant Character := Character'Val (10);
        Backspace_Key : constant Character := Character'Val (127);
    begin
        if Key = Escape_Key or else
          Selector.Last_Key_Event (1) = Escape_Key or else
          Selector.Last_Key_Event (2) = Escape_Key
        then
            case Key is
            when Down_Key =>
                Selector.Next_Selection;
            when Up_Key =>
                Selector.Previous_Selection;
            when others =>
                null;
            end case;
        else
            case Key is
            when Enter_Key | Ret_Key =>
                On_Selected_Callback
                  (To_Wide_Wide_String (Selector.Current_String));
            when Backspace_Key =>
                Selector.Delete_Character;
            when others =>
                Append (Selector.Current_String,
                        To_Wide_Wide_Character (Key));
                Selector.Current_Selection := 1;
            end case;
        end if;

        Widgets.Key_Event (Widget_Type (Selector), Key);
    end Key_Event;

    procedure Next_Selection (Selector : in out Selector_Type) is
        use Data_Vectors;
    begin
        if Selector.Current_Selection < Natural
          (Length (Selector.Current_Filter_Data))
        then
            Selector.Current_Selection := Selector.Current_Selection + 1;
        else
            Selector.Current_Selection := 1;
        end if;
    end Next_Selection;

    procedure Previous_Selection (Selector : in out Selector_Type) is
        use Data_Vectors;
    begin
        if Selector.Current_Selection > 1 then
            Selector.Current_Selection := Selector.Current_Selection - 1;
        else
            Selector.Current_Selection := Natural
              (Length (Selector.Current_Filter_Data));
        end if;
    end Previous_Selection;

    procedure Put_Data (Selector : Selector_Type) is
        use Console.SGR;
        A_String : Unbounded_Wide_Wide_String;
        Entry_Amount : constant Natural :=
            --  Add 2 lines for the top+bottom border if present.
            (if Selector.Config.Draw_Border = Widgets.Border_None then
                 Default_Height
             else
                 Default_Height - 2);

    begin
        for I in Selector.Current_Selection ..
                 Selector.Current_Selection + Entry_Amount
        loop
            A_String := To_Unbounded_Wide_Wide_String ("");
            if I <= Integer (Selector.Filtered_Data.Length) then
                A_String := Selector.Filtered_Data (I);
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

            if Selector.Config.Draw_Border /= Widgets.Border_None then
                Console.CSI_Codes.Cursor_Forward;
            end if;
            Put (To_Wide_Wide_String (A_String));
            Console.CSI_Codes.Cursor_Down;
            Console.CSI_Codes.Cursor_Horizontal (Selector.Row);
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

    procedure Update_Filtered_Data (Selector : in out Selector_Type;
                                    Substring : Wide_Wide_String)
    is
    begin
        Update_Filtered_Data (Selector,
                              To_Unbounded_Wide_Wide_String (Substring));
    end Update_Filtered_Data;

    procedure Update_Filtered_Data (Selector : in out Selector_Type;
                                    Substring : Unbounded_Wide_Wide_String)
    is
    begin
        Selector.Current_String := Substring;
        Selector.Filtered_Data := Selector.Filter_Data (Substring);
    end Update_Filtered_Data;

    procedure Update_Filtered_Data (Selector : in out Selector_Type)
    is
    begin
        Selector.Filtered_Data :=
            Selector.Filter_Data (Selector.Current_String);
    end Update_Filtered_Data;

    function User_Selected (Selector : in out Selector_Type) return Boolean is
        Enter_Key : constant Character := Character'Val (13);
        Ret_Key : constant Character := Character'Val (10);
    begin
        return Selector.Last_Key_Event (1) = Enter_Key or else
          Selector.Last_Key_Event (1) = Ret_Key;
    end User_Selected;

end Widgets.Selectors;
