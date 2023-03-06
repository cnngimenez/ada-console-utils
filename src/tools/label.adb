--  label.adb ---

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

with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions; --  To_Wide_Wide_String
with Ada.Text_IO;
use Ada.Text_IO;

with Ada_Tuis;
use Ada_Tuis;
with Mouse;
with Widgets;
with Widgets.Labels;
use Widgets.Labels;
with Widgets.Buttons;
use Widgets.Buttons;

procedure Label is
    Tui : Ada_Tui_Type;
    Text_Label : aliased Label_Type;
    Quit_Button : aliased Button_Type;
    Quit : Boolean := False;
    Message : Unbounded_String := To_Unbounded_String ("");
    Config : Widgets.Widget_Config_Type := Widgets.Default_Widget_Config;

    procedure Configure_Button;
    procedure Configure_Label;
    procedure Quit_Button_Click_Handler (Widget : Widgets.Widget_Type'Class;
                                         Mouse_Event : Mouse.Mouse_Event_Type);
    procedure Show_Help;
    procedure Read_Message;

    procedure Configure_Button is
    begin
        Config.Draw_Border := Widgets.Border_Simple;

        Quit_Button.Initialize ("Quit", 28, 10);
        Quit_Button.Set_Config (Config);
        Quit_Button.Set_Mouse_Click_Handler (
            Quit_Button_Click_Handler'Unrestricted_Access);
    end Configure_Button;

    procedure Configure_Label is
    begin
        Config.Draw_Border := Widgets.Border_Simple;

        Text_Label.Initialize (To_Wide_Wide_String (To_String (Message)),
                               2, 2, 25, 25);
        Text_Label.Set_Config (Config);
    end Configure_Label;

    procedure Quit_Button_Click_Handler (Widget : Widgets.Widget_Type'Class;
                                         Mouse_Event : Mouse.Mouse_Event_Type)
    is
        pragma Unreferenced (Widget, Mouse_Event);
    begin
        Quit := True;
    end Quit_Button_Click_Handler;

    procedure Read_Message is
    begin
        while not End_Of_File loop
            declare
                Line : constant String := Get_Line;
            begin
                Message := Message & To_Unbounded_String (Line);
            end;
        end loop;
    end Read_Message;

    procedure Show_Help is
    begin
        Put_Line ("Show a label with a message and a button.");
        New_Line;
        Put_Line ("Usage:");
        Put_Line ("      label MESSAGE");
        New_Line;
        Put_Line ("When MESSAGE is ""-"", read the message From "
            & "standard input, until EOF (Control d) reached.");
    end Show_Help;

begin
    if Argument_Count = 0 then
        Show_Help;
        return;
    end if;

    if Argument (1) = "-" then
        Read_Message;
    else
        Message := To_Unbounded_String (Argument (1));
    end if;

    Configure_Label;
    Configure_Button;

    Tui.Add_Widget (Text_Label'Unrestricted_Access);
    Tui.Add_Widget (Quit_Button'Unrestricted_Access);

    Tui.Start;
    loop
        Tui.Draw;
        Tui.Event_Loop;

        exit when Tui.Last_Key_Pressed = 'q' or else Quit;
    end loop;

end Label;
