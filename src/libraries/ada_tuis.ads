--  ada_tuis.ads ---

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

with Ada.Containers.Vectors;
with Mouse;
use Mouse;
with Widgets;
use Widgets;

package Ada_Tuis is

    type Ada_Tui_Type is tagged private;

    procedure Add_Widget (Tui : in out Ada_Tui_Type;
                          Widget : Widget_Access_Type);

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

    procedure Start (Tui : in out Ada_Tui_Type);

    procedure Draw (Tui : Ada_Tui_Type);
    --  Draw all Widgets.
    --
    --  Draw all widgets, and then position the cursor were it was.

    procedure Event_Loop (Tui : in out Ada_Tui_Type);
    --  Wait for an event and store it in the Tui Object.
    --
    --  Wait for any keyboard or mouse event and store it a the Tui.
    --  Call any widget handlers.

    function Last_Key_Pressed (Tui : Ada_Tui_Type) return String;
    --  Return the last key Pressed.

    function Last_Key_Pressed (Tui : Ada_Tui_Type) return Character;

private

    type Code_String_Type is new String (1 .. 12);

    package Widget_Vector_Pack is new Ada.Containers.Vectors (
        Index_Type => Positive,
        Element_Type => Widget_Access_Type);
    subtype Widget_Vector is Widget_Vector_Pack.Vector;

    type Ada_Tui_Type is tagged
    record
        Widgets : Widget_Vector;
        Last_Key_Pressed : String (1 .. 10) := "          ";
        --  Last_Key_Pressed (10) is the last Key.
        --
        --  A history of the last key pressed.
    end record;

    procedure Read_Escape_Codes (Codes : out Code_String_Type);
    procedure Process_Mouse_Event (Tui : Ada_Tui_Type;
                                   Mouse_Event : Mouse_Event_Type);

    procedure Call_On_Click_Handlers (Tui : Ada_Tui_Type;
                                      Mouse_Event : Mouse_Event_Type);
    --  Call all widget on_click Handlers.
    --
    --  Check if the click were on any widget and call their Handler.

    procedure Call_On_Mouse_Move_Handlers (Tui : Ada_Tui_Type;
                                           Mouse_Event : Mouse_Event_Type);
    --  Call all widgets on_mouse_move Handlers.
    --
    --  Check if the mouse is on any widget and call their handlers if
    --  the mouse is on Them.

    procedure Append_Last_Key (Tui : in out Ada_Tui_Type; Key : Character);

end Ada_Tuis;
