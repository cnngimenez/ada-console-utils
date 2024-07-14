--  apagerlib-commands.ads ---

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

with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded;

--
--  Manage keyboard and commands mappings.
--
package Apagerlib.Commands is

    package Command_Hashes is new Ada.Containers.Hashed_Maps
        (Key_Type => Unbounded_String,
         Element_Type => Unbounded_String,
         Hash => Ada.Strings.Unbounded.Hash,
         Equivalent_Keys => "=");
    subtype Command_Map is Command_Hashes.Map;

    function Default_Maps return Command_Map;
    --  procedure load_maps

end Apagerlib.Commands;
