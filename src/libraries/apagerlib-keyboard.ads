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

--  Manage the APager keyboard.
package Apagerlib.Keyboard is

    procedure Open_Keyboard;
    --  Find and open the TTY or PTS file associated with the keyboard.

    function Wait_For_Key return Character;

    procedure Close_Keyboard;

end Apagerlib.Keyboard;
