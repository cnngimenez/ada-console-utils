--  console-tty.adb ---

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

with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
use Interfaces.C.Strings;


package body Console.TTY is

    function Ttyname (Fd : Int) return chars_ptr;
    pragma Import (C, Ttyname, "ttyname");

    function TTY_Name (Fd : Integer) return String is
        Ptr : chars_ptr;
    begin
        Ptr := Ttyname (Int (Fd));
        if Ptr /= Null_Ptr then
            return Value (Ptr);
        else
            return "";
        end if;
    end TTY_Name;

end Console.TTY;
