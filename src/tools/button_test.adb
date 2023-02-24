--  button_test.adb ---

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
use Ada.Text_IO;
with Ada_Tuis;
use Ada_Tuis;
with Mouse;
with Widgets;
with Widgets.Buttons;
use Widgets.Buttons;
with Widgets.Labels;
use Widgets.Labels;

procedure Button_Test is

    procedure Hello_Click (Mouse_Event : Mouse.Mouse_Event_Type);

    Tui : Ada_Tui_Type;
    Hello_Label : aliased Label_Type;
    Hello_Button : aliased Button_Type;

    procedure Hello_Click (Mouse_Event : Mouse.Mouse_Event_Type) is
        pragma Unreferenced (Mouse_Event);
    begin
        --  Hello_Label.Set_Text ("The button was Clicked!");
        Put_Line ("Trying a simple put_line");
    end Hello_Click;

    Click_Handler : constant Widgets.Mouse_Handler :=
        Hello_Click'Unrestricted_Access;

    --  W : Widgets.Widget_Access_Type;
begin
    Hello_Label.Initialize ("Welcome!", 2,  2, 10, 3);
    Hello_Button.Initialize ("Press button!", 10, 10);
    Hello_Button.Set_Mouse_Click_Handler (Click_Handler);

    Tui.Add_Widget (Hello_Label'Unrestricted_Access);
    Tui.Add_Widget (Hello_Button'Unrestricted_Access);

    Tui.Start;
    loop
        Tui.Draw;
        Tui.Event_Loop;

        exit when Tui.Last_Key_Pressed = 'q';
    end loop;

end Button_Test;
