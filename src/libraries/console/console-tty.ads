--  console-tty.ads ---

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

package Console.TTY is

    pragma Linker_Options ("-lc");

    Standard_Input_Fd : constant Integer := 0;
    Standard_Output_Fd : constant Integer := 1;
    Standard_Error_Fd : constant Integer := 2;
    --  See: man 3 Stdin

    --  function TTY_Slot return Integer;

    --  --  isatty is already on System.CRTL

    function TTY_Name (Fd : Integer) return String;

    --  function PTS_Name (Fd : Integer) return String;

    --  function Getpt return Integer;

    --  function Current_Term_ID return String;
    --  --  ctermid (NULL)

    --  function Current_Term_ID (S : String) return String;
    --  --  ctermid

end Console.TTY;
