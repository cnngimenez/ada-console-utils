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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

package body Widgets is
    procedure Draw (Widget : in out Widget_Type) is
    begin
        null;
    end Draw;

    function Get_Height (Widget : Widget_Type) return Natural
    is (Widget.Height);

    function Get_Column (Widget : Widget_Type) return Natural
    is (Widget.Column);

    function Get_Row (Widget : Widget_Type) return Natural
    is (Widget.Row);

    function Get_Width (Widget : Widget_Type) return Natural
    is (Widget.Width);

    procedure Initialize (Widget : in out Widget_Type;
                          Row, Column, Width, Height : Natural) is
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

end Widgets;
