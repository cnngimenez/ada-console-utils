--  apager-keyboard.adb ---

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

with Ada.Text_IO;
use Ada.Text_IO;
with Console.TTY;
use Console.TTY;

package body Apagerlib.Keyboard is

    Keyboard_File : File_Type;

    procedure Close_Keyboard is
    begin
        Close (Keyboard_File);
    end Close_Keyboard;

    procedure Open_Keyboard is
    begin
        Open (Keyboard_File, In_File, TTY_Name (Standard_Error_Fd));
    end Open_Keyboard;

    function Wait_For_Key return Character is
        C : Character;
    begin
        Get_Immediate (Keyboard_File, C);
        return C;
    end Wait_For_Key;

end Apagerlib.Keyboard;
