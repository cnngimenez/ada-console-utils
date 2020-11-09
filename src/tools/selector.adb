--  selector.adb ---

--  Copyright 2020 cnngimenez
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

with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Characters.Conversions;
use Ada.Characters.Conversions;

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

with Widgets.Selectors;
use Widgets.Selectors;


procedure Selector is
    Selector : Selector_Type;
begin
    if Argument_Count = 0 then
        return;
    end if;

    for I in 1 .. Argument_Count loop
        Selector.Add (To_Wide_Wide_String (Argument (I)));
    end loop;

    Selector.Execute;

    Put_Line (Selector.Get_Current_String);

end Selector;
