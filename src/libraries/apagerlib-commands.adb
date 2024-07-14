--  apagerlib-commands.adb ---

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

package body Apagerlib.Commands is

    function Tou (Str : String) return Unbounded_String is
     (To_Unbounded_String (Str));

    function Default_Maps return Command_Map is
        Map : Command_Map;
    begin
        Map.Insert (Tou ("C-x C-c"), Tou ("quit");
        Map.Insert (Tou ("M-x"), Tou ("execute-extended-Command"));
        --  Navigation
        Map.Insert (Tou ("h") , Tou ("previous-line"));
        Map.Insert (Tou ("j") , Tou ("previous-line"));
        Map.Insert (Tou ("k") , Tou ("previous-line"));
        Map.Insert (Tou ("l") , Tou ("previous-Line"));

        Map.Insert (Tou ("M-g g") , Tou ("goto-line"));
    end Default_Maps;

end Apagerlib.Commands;
