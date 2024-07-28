--  console.geometry.adb ---

--  Copyright 2024 cnngimenez
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

with Ada.Environment_Variables;

package body Console.Geometry is

    function Get_Columns return Positive is
    begin
        if Ada.Environment_Variables.Exists ("COLUMNS") then
            return Integer'Value
                (Ada.Environment_Variables.Value ("COLUMNS"));
        else
            raise No_Geometry_Information;
        end if;
    end Get_Columns;

    function Get_Lines return Positive is
    begin
        if Ada.Environment_Variables.Exists ("LINES") then
            return Integer'Value (Ada.Environment_Variables.Value ("LINES"));
        else
            raise No_Geometry_Information;
        end if;
    end Get_Lines;

end Console.Geometry;
