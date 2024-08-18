--  apagerlib-frontend-modelines.ads ---

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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

package Apagerlib.Frontend.Modelines is

    type Truncate_Type is (Truncate, Visual_Line);
    --  type Show_Options is (Top_Byte, Truncate, Filename);

    type Modeline_Type is tagged record
        --  Show : array (Show_Options'Range) of Boolean;

        Top_Byte : Positive;
        Truncate : Truncate_Type;
        Filename : Unbounded_String;

        More_Text : Unbounded_String;
        --  Any extra information the user may want.

        Line_Position : Positive;
        --  Modeline position (where it should be printed?)
        Width : Positive;
        --  Modeline width
    end record;

    function Modeline_String (Modeline : Modeline_Type)
        return Unbounded_String;

    procedure Put_Modeline (Modeline : Modeline_Type);

    Default_Modeline : constant Modeline_Type := (
        --  Show => (Top_Byte => True, Truncate => True, Filename => True),
        Top_Byte => 1,
        Truncate => Visual_Line,
        Filename => To_Unbounded_String ("*Scratch*"),
        More_Text => To_Unbounded_String (""),
        Line_Position => 25,
        Width => 25);

end Apagerlib.Frontend.Modelines;
