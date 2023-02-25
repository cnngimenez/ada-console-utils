--  ada_tuis.adb ---

--  Copyright 2023 cnngimenez
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

with Ada.Text_IO;
with Ada.Characters.Latin_1;
with Console;

package body Ada_Tuis is
    procedure Add_Widget (Tui : in out Ada_Tui_Type;
                          Widget : Widget_Access_Type)
    is
        use Widget_Vector_Pack;
    begin
        Append (Tui.Widgets, Widget);
    end Add_Widget;

    --  procedure Remove_Widget (Tui : in out Ada_Tui_Type;
    --                           Widget_Name : String);

    --  procedure Remove_Widget (Tui : in out Ada_Tui_Type;
    --                           Widget_Number : Natural);

    --  function Get_Widget (Tui : Ada_Tui_Type;
    --                       Widget_Name : String)
    --                       return Widget_Type;

    --  function Get_Widget (Tui : Ada_Tui_Type;
    --                       Widget_Number : Natural)
    --                       return Widget_Type;

    procedure Append_Last_Key (Tui : in out Ada_Tui_Type; Key : Character)
    is
    begin
        for I in Tui.Last_Key_Pressed'First + 1 .. Tui.Last_Key_Pressed'Last
        loop
            Tui.Last_Key_Pressed (I - 1) := Tui.Last_Key_Pressed (I);
        end loop;

        Tui.Last_Key_Pressed (10) := Key;
    end Append_Last_Key;

    procedure Call_On_Click_Handlers (Tui : Ada_Tui_Type;
                                      Mouse_Event : Mouse_Event_Type)
    is
    begin
        for Widget : Widget_Access_Type of Tui.Widgets loop
            if Is_Mouse_In_Widget (Widget.all, Mouse_Event) then
                Call_Mouse_Click_Handler (Widget.all, Mouse_Event);
            end if;
        end loop;
    end Call_On_Click_Handlers;

    procedure Call_On_Mouse_Move_Handlers (Tui : Ada_Tui_Type;
                                           Mouse_Event : Mouse_Event_Type)
    is
    begin
        for Widget : Widget_Access_Type of Tui.Widgets loop
            if Is_Mouse_In_Widget (Widget.all, Mouse_Event) then
                Call_Mouse_Move_Handler (Widget.all, Mouse_Event);
            end if;
        end loop;
    end Call_On_Mouse_Move_Handlers;

    procedure Draw (Tui : Ada_Tui_Type) is
    begin
        Console.Save_Cursor_Position;

        for Widget : Widget_Access_Type of Tui.Widgets loop
            Draw (Widget.all);
        end loop;

        Console.Restore_Cursor_Position;
    end Draw;

    procedure Event_Loop (Tui : in out Ada_Tui_Type) is
        use Ada.Text_IO;
        use Ada.Characters.Latin_1;

        C : Character;
        Codes : Code_String_Type;
        Mouse_Event : Mouse_Event_Type;
    begin
        Mouse.Enable_Mouse;
        Get_Immediate (C);

        if C = ESC then
            Read_Escape_Codes (Codes);
            Mouse.Disable_Mouse;

            Mouse_Event := Mouse.String_To_Event (String (Codes));
            Process_Mouse_Event (Tui, Mouse_Event);
        else
            --  No mouse event: it must be a keyboard!
            Mouse.Disable_Mouse;
            Append_Last_Key (Tui, C);
        end if;

    end Event_Loop;

    function Last_Key_Pressed (Tui : Ada_Tui_Type) return String
        is (String (Tui.Last_Key_Pressed));

    function Last_Key_Pressed (Tui : Ada_Tui_Type) return Character
        is (Tui.Last_Key_Pressed (Tui.Last_Key_Pressed'Last));

    procedure Process_Mouse_Event (Tui : Ada_Tui_Type;
                                   Mouse_Event : Mouse_Event_Type)
    is
    begin
        if Mouse_Event.Invalid then
            return;
        end if;

        if Mouse_Event.Release = False
            and then (Mouse_Event.Button_1_Pressed
                or else Mouse_Event.Button_2_Pressed
                or else Mouse_Event.Button_3_Pressed)
        then
            Tui.Call_On_Click_Handlers (Mouse_Event);
        else
            Tui.Call_On_Mouse_Move_Handlers (Mouse_Event);
        end if;
    end Process_Mouse_Event;

    procedure Read_Escape_Codes (Codes : out Code_String_Type) is
        use Ada.Text_IO;
        use Ada.Characters.Latin_1;

        C : Character;
        I : Positive := Codes'First + 1;
    begin
        Codes (Codes'First) := ESC;

        loop
            Get_Immediate (C);

            Codes (I) := C;
            I := I + 1;

            exit when C = 'm' or else C = 'M' or else I > Codes'Last;
        end loop;
    end Read_Escape_Codes;

    procedure Start (Tui : in out Ada_Tui_Type) is
    begin
        Tui.Last_Key_Pressed := "          ";
        Console.Erase_Display (Console.Entire_Screen);
    end Start;

end Ada_Tuis;
