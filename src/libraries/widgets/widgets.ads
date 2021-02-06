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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

package Widgets is

    type Widget_Type is tagged private;

    procedure Initialize (Widget : in out Widget_Type;
                          Row, Column, Width, Height : Natural);

    function Get_Width (Widget : Widget_Type) return Natural;
    function Get_Height (Widget : Widget_Type) return Natural;
    function Get_Row (Widget : Widget_Type) return Natural;
    function Get_Column (Widget : Widget_Type) return Natural;

    procedure Resize (Widget : in out Widget_Type; Width, Height : Natural);
    procedure Move (Widget : in out Widget_Type; Row, Column : Natural);

    procedure Draw (Widget : in out Widget_Type);

    --  ----------
    --  Events
    --  ----------

    procedure Key_Event (Widget : in out Widget_Type; Key : Character);

private
    --  1 : The last key pressed.
    --  2 : The 2nd previous key pressed.
    type Last_Event_Array_Type is array (1 .. 2) of Character;

    type Widget_Type is tagged record
        Row, Column, Width, Height : Natural;
        Last_Key_Event : Last_Event_Array_Type;
    end record;
end Widgets;
