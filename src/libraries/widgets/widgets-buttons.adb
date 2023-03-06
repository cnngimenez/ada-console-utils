--  widgets-buttons.adb ---

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

with Ada.Wide_Wide_Text_IO;
--  with Console;

package body Widgets.Buttons is

    procedure Default_Mouse_Move_Handler (Widget : in out Widget_Type'Class;
                                          Mouse_Event : Mouse_Event_Type)
    is
    begin
        Widget.Invert_Colours := Is_Mouse_In_Widget (Widget, Mouse_Event);
        Draw (Button_Type (Widget));
    end Default_Mouse_Move_Handler;

    overriding procedure Draw (Button : in out Button_Type) is
        --  use Console;
        use Ada.Wide_Wide_Text_IO;
    begin
        Widgets.Draw (Widget_Type (Button));
        --  Cursor_Position (Button.Row, Button.Column);

        Put (To_Wide_Wide_String (Button.Text));
    end Draw;

    function Get_Text (Button : Button_Type) return Unbounded_Wide_Wide_String
        is (Button.Text);

    function Get_Text (Button : Button_Type) return Wide_Wide_String
        is (To_Wide_Wide_String (Button.Text));

    procedure Initialize (Button : in out Button_Type;
                          Text : Wide_Wide_String;
                          Row, Column : Natural) is
    begin
        Initialize (Button, To_Unbounded_Wide_Wide_String (Text),
                    Row, Column);
    end Initialize;

    procedure Initialize (Button : in out Button_Type;
                          Text : Unbounded_Wide_Wide_String;
                          Row, Column : Natural) is
    begin
        Widgets.Initialize (Widget_Type (Button),
                            Row, Column,
                            Default_Width, Default_Height);

        Button.Text := Text;
        Button.Set_Mouse_Move_Handler (Default_Mouse_Move_Handler'Access);
    end Initialize;

    procedure Set_Text (Button : in out Button_Type;
                        Text : Wide_Wide_String) is
    begin
        Set_Text (Button, To_Unbounded_Wide_Wide_String (Text));
    end Set_Text;

    procedure Set_Text (Button : in out Button_Type;
                        Text : Unbounded_Wide_Wide_String) is
    begin
        Button.Text := Text;
    end Set_Text;

end Widgets.Buttons;
