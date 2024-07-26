--  apager_test.adb ---

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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Apagerlib.Backend; use Apagerlib.Backend;
with Apagerlib.Memories; use Apagerlib.Memories;
with Apagerlib.File_Backend; use Apagerlib.File_Backend;

procedure Apager_Backend_Test is
    Memory : access Backend_Stream'Class;
    File1 : aliased File_Backend;
    Stdin : aliased Page_Memory;
begin
    if Argument_Count > 0 then
        File1.Set_Filename (Argument (1));
        Memory := File1'Access;
    else
        Memory := Stdin'Access;
    end if;

    Memory.Open;

    Put (Memory.Get_Char);
    while not Memory.End_Of_File loop
        Put (Memory.Next_Char);
    end loop;

    Memory.Close;

end Apager_Backend_Test;
