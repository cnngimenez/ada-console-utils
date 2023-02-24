--  widgets-buttons.ads ---

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

with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;

package Widgets.Buttons is

    Default_Height : constant Natural := 1;
    Default_Width : constant Natural := 7;

    type Button_Type is new Widget_Type with private;

    procedure Initialize (Button : in out Button_Type;
                          Text : Wide_Wide_String;
                          Row, Column : Natural);

    procedure Initialize (Button : in out Button_Type;
                          Text : Unbounded_Wide_Wide_String;
                          Row, Column : Natural);

    procedure Set_Text (Button : in out Button_Type;
                        Text : Wide_Wide_String);
    procedure Set_Text (Button : in out Button_Type;
                        Text : Unbounded_Wide_Wide_String);

    function Get_Text (Button : Button_Type) return Unbounded_Wide_Wide_String;
    function Get_Text (Button : Button_Type) return Wide_Wide_String;

    overriding procedure Draw (Button : in out Button_Type);

private

    type Button_Type is new Widget_Type with
    record
        Text : Unbounded_Wide_Wide_String;
    end record;

end Widgets.Buttons;
