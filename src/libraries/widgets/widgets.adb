--  widgets.adb ---

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

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

package body Widgets is

    procedure Call_Mouse_Click_Handler (Widget : Widget_Type;
                                        Mouse_Event : Mouse_Event_Type)
    is
    begin
        if Widget.Mouse_Click_Handler = null then
            return;
        end if;

        Widget.Mouse_Click_Handler (Mouse_Event);
    end Call_Mouse_Click_Handler;

    procedure Call_Mouse_Move_Handler (Widget : Widget_Type;
                                       Mouse_Event : Mouse_Event_Type)
    is
    begin
        if Widget.Mouse_Move_Handler = null then
            return;
        end if;

        Widget.Mouse_Move_Handler (Mouse_Event);
    end Call_Mouse_Move_Handler;

    procedure Draw (Widget : in out Widget_Type) is
    begin
        if Widget.Config.Draw_Border = Border_Simple then
            Draw_Border (Widget);
            Console.Cursor_Position (Widget.Row + 1, Widget.Column + 1);
        else
            Console.Cursor_Position (Widget.Row, Widget.Column);
        end if;

        Console.Set_RGB_Colour (Widget.Config.Foreground_Colour);
        Console.Set_RGB_Background (Widget.Config.Background_Colour);
    end Draw;

    procedure Draw_Border (Widget : Widget_Type) is
    begin
        Console.Cursor_Position (Widget.Row, Widget.Column);
        Console.Set_RGB_Colour (Widget.Config.Border_Foreground_Colour);
        Console.Set_RGB_Background (Widget.Config.Border_Background_Colour);

        Put ("┌");
        for I in (Widget.Column + 1) .. (Widget.Column + Widget.Width - 2) loop
            Put ("─");
        end loop;
        Put_Line ("┐");

        for I in (Widget.Row + 1) .. (Widget.Row + Widget.Height)
        loop
            Console.Cursor_Horizontal (Widget.Column);
            Put ("│");
            Console.Cursor_Horizontal (Widget.Column + Widget.Width - 1);
            Put_Line ("│");
        end loop;

        Console.Cursor_Horizontal (Widget.Column);
        Put ("└");
        for I in (Widget.Column + 1) .. (Widget.Column + Widget.Width - 2) loop
            Put ("─");
        end loop;
        Put ("┘");
    end Draw_Border;

    function Get_Height (Widget : Widget_Type) return Natural
    is (Widget.Height);

    function Get_Column (Widget : Widget_Type) return Natural
    is (Widget.Column);

    function Get_Config (Widget : Widget_Type) return Widget_Config_Type
    is (Widget.Config);

    function Get_Row (Widget : Widget_Type) return Natural
    is (Widget.Row);

    function Get_Width (Widget : Widget_Type) return Natural
    is (Widget.Width);

    procedure Initialize (Widget : in out Widget_Type;
                          Row, Column, Width, Height : Natural;
                          Config : Widget_Config_Type :=
                              Default_Widget_Config)
    is
    begin
        Widget.Row := Row;
        Widget.Column := Column;
        Widget.Width := Width;
        Widget.Height := Height;
        Widget.Mouse_Move_Handler := null;
        Widget.Mouse_Click_Handler := null;

        for I in Widget.Last_Key_Event'Range loop
            Widget.Last_Key_Event (I) := Character'Val (0);
        end loop;
    end Initialize;

    function Is_Coordinates_In_Widget (Widget : Widget_Type; X, Y : Natural)
        return Boolean
        is (X >= Widget.Column
            and then X <= Widget.Column + Widget.Width
            and then Y >= Widget.Row
            and then Y <= Widget.Row + Widget.Height);

    procedure Key_Event (Widget : in out Widget_Type; Key : Character) is
    begin
        Widget.Last_Key_Event (2) := Widget.Last_Key_Event (1);
        Widget.Last_Key_Event (1) := Key;
    end Key_Event;

    procedure Mouse_Event (Widget : in out Widget_Type;
                           Mouse_Event : Mouse_Event_Type)
    is
    begin
        if not Widget.Is_Mouse_In_Widget (Mouse_Event.X, Mouse_Event.Y) then
            return;
        end if;

        if Mouse_Event.Button_1_Pressed
            or else Mouse_Event.Button_2_Pressed
            or else Mouse_Event.Button_3_Pressed
        then
            Widget.Mouse_Click_Handler (Mouse_Event);
        else
            Widget.Mouse_Move_Handler (Mouse_Event);
        end if;
    end Mouse_Event;

    procedure Move (Widget : in out Widget_Type; Row, Column : Natural) is
    begin
        Widget.Row := Row;
        Widget.Column := Column;
    end Move;

    procedure Remove_Mouse_Click_Handler (Widget : in out Widget_Type)
    is
    begin
        Widget.Mouse_Click_Handler := null;
    end Remove_Mouse_Click_Handler;

    procedure Remove_Mouse_Move_Handler (Widget : in out Widget_Type)
    is
    begin
        Widget.Mouse_Move_Handler := null;
    end Remove_Mouse_Move_Handler;

    procedure Resize (Widget : in out Widget_Type; Width, Height : Natural)
    is begin
        Widget.Width := Width;
        Widget.Height := Height;
    end Resize;

    procedure Set_Column (Widget : in out Widget_Type; Column : Natural) is
    begin
        Widget.Column := Column;
    end Set_Column;

    procedure Set_Config (Widget : in out Widget_Type;
                          Config : Widget_Config_Type)
    is
    begin
        Widget.Config := Config;
    end Set_Config;

    procedure Set_Height (Widget : in out Widget_Type; Height : Natural) is
    begin
        Widget.Height := Height;
    end Set_Height;

    procedure Set_Mouse_Click_Handler (Widget : in out Widget_Type;
                                       Handler : Mouse_Handler) is
    begin
        Widget.Mouse_Click_Handler := Handler;
    end Set_Mouse_Click_Handler;

    procedure Set_Mouse_Move_Handler (Widget : in out Widget_Type;
                                      Handler : Mouse_Handler) is
    begin
        Widget.Mouse_Move_Handler := Handler;
    end Set_Mouse_Move_Handler;

    procedure Set_Row (Widget : in out Widget_Type; Row : Natural) is
    begin
        Widget.Row := Row;
    end Set_Row;

    procedure Set_Width (Widget : in out Widget_Type; Width : Natural) is
    begin
        Widget.Width := Width;
    end Set_Width;

end Widgets;
