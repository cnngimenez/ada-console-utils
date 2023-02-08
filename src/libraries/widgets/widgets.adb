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

with Console;
with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

package body Widgets is
    procedure Draw (Widget : in out Widget_Type) is
    begin
        if Widget.Config.Draw_Border = Border_Simple then
            Draw_Border (Widget);
            Console.Cursor_Position (Widget.Row + 2, Widget.Column + 1);
        else
            Console.Cursor_Position (Widget.Row, Widget.Column);
        end if;
    end Draw;

    procedure Draw_Border (Widget : Widget_Type) is
    begin
        Console.Cursor_Position (Widget.Row, Widget.Column);

        Put ("┌");
        for I in (Widget.Column + 1) .. (Widget.Column + Widget.Width - 2) loop
            Put ("─");
        end loop;
        Put_Line ("┐");

        for I in (Widget.Row + 1) .. (Widget.Row + Widget.Height - 1)
        loop
            Put ("│");
            Console.Cursor_Horizontal (Widget.Width);
            Put_Line ("│");
        end loop;

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

        for I in Widget.Last_Key_Event'Range loop
            Widget.Last_Key_Event (I) := Character'Val (0);
        end loop;
    end Initialize;

    procedure Key_Event (Widget : in out Widget_Type; Key : Character) is
    begin
        Widget.Last_Key_Event (2) := Widget.Last_Key_Event (1);
        Widget.Last_Key_Event (1) := Key;
    end Key_Event;

    procedure Move (Widget : in out Widget_Type; Row, Column : Natural) is
    begin
        Widget.Row := Row;
        Widget.Column := Column;
    end Move;

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

    procedure Set_Row (Widget : in out Widget_Type; Row : Natural) is
    begin
        Widget.Row := Row;
    end Set_Row;

    procedure Set_Width (Widget : in out Widget_Type; Width : Natural) is
    begin
        Widget.Width := Width;
    end Set_Width;

end Widgets;
