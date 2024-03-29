--  selector.adb ---

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

with Ada.Command_Line;
use Ada.Command_Line;
--  with Ada.Characters.Conversions;
--  use Ada.Characters.Conversions;

with Ada.Text_IO;

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

with Console.CSI_Codes;
use Console.CSI_Codes;

with Widgets;
with Widgets.Selectors;

procedure Selector is
    procedure Selected_Callback (Current_String : Wide_Wide_String);
    procedure Show_Help;

    End_Program : Boolean := False;

    procedure Selected_Callback (Current_String : Wide_Wide_String) is
        pragma Unreferenced (Current_String);
    begin
        End_Program := True;
    end Selected_Callback;

    procedure Show_Help is
    begin
        Put_Line ("selector: Show several options for the user to choose.");
        New_Line;
        Put_Line ("Synopsis:");
        New_Line;
        Put_Line ("    selector FILE_WITH_OPTIONS");
    end Show_Help;

    package Myselector is new Widgets.Selectors
      (On_Selected_Callback => Selected_Callback);

    use Myselector;

    Widget_Config : constant Widgets.Widget_Config_Type := (
        Draw_Border => Widgets.Border_Simple,
        Border_Foreground_Colour => Widgets.Default_Border_Foreground_Colour,
        Border_Background_Colour => Widgets.Default_Border_Background_Colour,
        Background_Colour => Widgets.Default_Background_Colour,
        Foreground_Colour => Widgets.Default_Foreground_Colour
    );
    Selector : Selector_Type;
    File : File_Type;
    Key : Character;
begin
    if Argument_Count = 0 then
        Show_Help;
        return;
    end if;

    Selector.Initialize (0, 0);
    Widgets.Widget_Type (Selector).Set_Config (Widget_Config);

    Open (File, In_File, Argument (1));
    while not End_Of_File (File) loop
        Selector.Add (Get_Line (File));
    end loop;
    Close (File);

    while not End_Program loop
        Erase_Display (Entire_Screen);
        Selector.Draw;
        Ada.Text_IO.Get_Immediate (Key);
        Selector.Key_Event (Key);
    end loop;

    Put_Line ("Result:");
    Put_Line (Selector.Get_Selected_String);

end Selector;
