--  apagerlib-file_backend.adb ---

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

package body Apagerlib.File_Backend is

    overriding
    procedure Close (Stream : in out File_Backend) is
    begin
        Ada.Text_IO.Close (Stream.File);
    end Close;

    overriding
    function End_Of_File (Stream : File_Backend) return Boolean is
    begin
        return Ada.Text_IO.End_Of_File (Stream.File);
    end End_Of_File;

    overriding
    function Get_Char (Stream : in out File_Backend) return Character
        is (Stream.Current_Character);

    overriding
    procedure Next_Char (Stream : in out File_Backend) is
        C : Character;
    begin
        if Ada.Text_IO.End_Of_File (Stream.File) then
            Stream.Current_Character := Character'Val (0);
            raise Apagerlib.Backend.No_More_Char;
        else
            Ada.Text_IO.Get_Immediate (Stream.File, C);
            Stream.Current_Character := C;
        end if;
    end Next_Char;

    overriding
    procedure Open (Stream : in out File_Backend) is
    begin
        Ada.Text_IO.Open (Stream.File,
                          Ada.Text_IO.In_File,
                          To_String (Stream.Filename));
        Stream.Next_Char;
    end Open;

    procedure Set_Filename (Stream : in out File_Backend; Name : String) is
    begin
        Stream.Filename := To_Unbounded_String (Name);
    end Set_Filename;

end Apagerlib.File_Backend;
