--  apager-keyboard.ads ---

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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

--
--  Manage the APager keyboard.
--
package Apagerlib.Keyboard is

    procedure Open_Keyboard;
    --  Find and open the TTY or PTS file associated with the keyboard.

    function Wait_For_Key return Character;

    function Wait_For_Strkey return Unbounded_String;
    --  Read keys and return a more human-readable Characters.

    function Get_Line return Unbounded_String;

    function CSI_To_Key (Chars : String) return String;

    function To_Strkey (Chars : String) return String;
    --  Convert a keyboard chars to human-readable Characters.
    --
    --  Example, convert ^[A characters into "<Up>".

    function Key_To_Graphical (Chars : String) return String;
    --  Convert a single character key into a more graphical Notation.
    --
    --  For example: "^x " (24) to "C-x".

    function To_Strkey (Chars : Unbounded_String) return Unbounded_String is
        (Ada.Strings.Unbounded.To_Unbounded_String
            (To_Strkey (Ada.Strings.Unbounded.To_String (Chars))));

    function To_Strkey (Chars : Unbounded_String) return String is
        (To_Strkey (Ada.Strings.Unbounded.To_String (Chars)));

    procedure Close_Keyboard;

end Apagerlib.Keyboard;
