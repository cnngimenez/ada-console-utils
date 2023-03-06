--  widgets.ads ---

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
--  along with this program.  If not, see <http://www.gnu.org/Licenses/>.

-------------------------------------------------------------------------

with Mouse;
use Mouse;
with Console;

--
--  Base Widget
--
--
--  Common behaviour and information for all Widgets.
--  Default events and configuration are alse defined here.
package Widgets is

    --  ---------------
    --  Configuration
    --  ---------------

    type Border_Type is (Border_None, Border_Simple);

    type Widget_Config_Type is record
        Draw_Border : Border_Type := Border_None;
        Border_Foreground_Colour : Console.RGB_Colour_Type;
        Border_Background_Colour : Console.RGB_Colour_Type;
        Background_Colour : Console.RGB_Colour_Type;
        Foreground_Colour : Console.RGB_Colour_Type;
    end record;

    Default_Border_Foreground_Colour : constant Console.RGB_Colour_Type := (
        Red => 255,
        Green => 100,
        Blue => 255
    );
    Default_Border_Background_Colour : constant Console.RGB_Colour_Type := (
        Red => 10,
        Green => 10,
        Blue => 10
    );
    Default_Background_Colour : constant Console.RGB_Colour_Type := (
        Red => 0,
        Green => 0,
        Blue => 0
    );
    Default_Foreground_Colour : constant Console.RGB_Colour_Type := (
        Red => 255,
        Green => 255,
        Blue => 255
    );

    Default_Widget_Config : constant Widget_Config_Type := (
        Draw_Border => Border_None,
        Border_Foreground_Colour => Default_Border_Foreground_Colour,
        Border_Background_Colour => Default_Border_Background_Colour,
        Background_Colour => Default_Background_Colour,
        Foreground_Colour => Default_Foreground_Colour
    );

    type Widget_Type is tagged private;
    type Widget_Access_Type is access all Widget_Type'Class;

    procedure Initialize (Widget : in out Widget_Type;
                          Row, Column, Width, Height : Natural;
                          Config : Widget_Config_Type :=
                              Default_Widget_Config);

    function Get_Width (Widget : Widget_Type) return Natural;
    function Get_Height (Widget : Widget_Type) return Natural;
    function Get_Row (Widget : Widget_Type) return Natural;
    function Get_Column (Widget : Widget_Type) return Natural;
    function Get_Config (Widget : Widget_Type) return Widget_Config_Type;

    procedure Set_Height (Widget : in out Widget_Type; Height : Natural);
    procedure Set_Width (Widget : in out Widget_Type; Width : Natural);
    procedure Set_Row (Widget : in out Widget_Type; Row : Natural);
    procedure Set_Column (Widget : in out Widget_Type; Column : Natural);
    procedure Set_Config (Widget : in out Widget_Type;
                          Config : Widget_Config_Type);

    procedure Resize (Widget : in out Widget_Type; Width, Height : Natural);
    procedure Move (Widget : in out Widget_Type; Row, Column : Natural);

    procedure Draw (Widget : in out Widget_Type);

    function Is_Coordinates_In_Widget (Widget : Widget_Type; X, Y : Natural)
        return Boolean;
    --  Are the point X, Y inside the Widget?
    --
    --  Return true if the point coordinate is inside the Widget
    --  coordinates.

    function Is_Mouse_In_Widget (Widget : Widget_Type; X, Y : Natural)
        return Boolean
        is (Is_Coordinates_In_Widget (Widget, X, Y));

    function Is_Mouse_In_Widget (Widget : Widget_Type;
                                 Mouse_Event : Mouse_Event_Type)
                                 return Boolean
        is (Is_Mouse_In_Widget (Widget, Mouse_Event.X, Mouse_Event.Y));

    --  ----------
    --  Events
    --  ----------

    type Mouse_Handler is access procedure (Widget : in out Widget_Type'Class;
                                            Mouse_Event : Mouse_Event_Type);

    procedure Key_Event (Widget : in out Widget_Type; Key : Character);

    procedure Mouse_Event (Widget : in out Widget_Type;
                           Mouse_Event : Mouse_Event_Type);
    --  What to do when the mouse is moved in or out this Widget.
    --
    --  Call Mouse_Move_Event or Mouse_Clicked_Event if the mouse is
    --  inside the Widget.
    --
    --  This subprogram can be overriden, but ensure to call
    --  Widget.Mouse_Event for the other two event Handling.

    procedure Set_Mouse_Click_Handler (Widget : in out Widget_Type;
                                       Handler : Mouse_Handler);

    procedure Remove_Mouse_Click_Handler (Widget : in out Widget_Type);

    procedure Set_Mouse_Move_Handler (Widget : in out Widget_Type;
                                      Handler : Mouse_Handler);

    procedure Remove_Mouse_Move_Handler (Widget : in out Widget_Type);

    procedure Call_Mouse_Move_Handler (Widget : in out Widget_Type;
                                       Mouse_Event : Mouse_Event_Type);
    --  Call the Mouse_Move_Handler procedure.

    procedure Call_Mouse_Click_Handler (Widget : in out Widget_Type;
                                        Mouse_Event : Mouse_Event_Type);
    --  Call the Mouse_Clicked_Handler procedure.

private
    --  1 : The last key pressed.
    --  2 : The 2nd previous key pressed.
    type Last_Event_Array_Type is array (1 .. 2) of Character;

    type Widget_Type is tagged record
        Row, Column, Width, Height : Natural;
        Last_Key_Event : Last_Event_Array_Type;
        Config : Widget_Config_Type;

        Invert_Colours : Boolean := False;
        --  Invert colours means to use foreground as background and
        --  viceversa.  This is useful for selections, focus, or
        --  providing a fast user feedback when something is Happening
        --  with the widget.

        --  Event handlers:

        Mouse_Move_Handler : Mouse_Handler := null;
        Mouse_Click_Handler : Mouse_Handler := null;
    end record;

    procedure Draw_Border (Widget : Widget_Type);

end Widgets;
