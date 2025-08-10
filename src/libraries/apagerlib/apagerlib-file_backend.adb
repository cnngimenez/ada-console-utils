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
        Char_IO.Close (Stream.File);
    end Close;

    overriding
    function Current_Position (Stream : File_Backend) return Positive
        is (Positive (Char_IO.Index (Stream.File)) - 1);

    overriding
    function End_Of_File (Stream : File_Backend) return Boolean is
    begin
        return Char_IO.End_Of_File (Stream.File);
    end End_Of_File;

    overriding
    procedure End_Position (Stream : in out File_Backend) is
    begin
        Stream.Set_Position (Positive (Char_IO.Size (Stream.File)));
        Char_IO.Read (Stream.File, Stream.Current_Character);
    end End_Position;

    overriding
    function Get_Char (Stream : in out File_Backend) return Character
        is (Stream.Current_Character);

    overriding
    procedure Next_Char (Stream : in out File_Backend) is
    begin
        if Char_IO.End_Of_File (Stream.File) then
            Stream.Current_Character := Character'Val (0);
            raise Apagerlib.Backend.No_More_Char;
        end if;

        Char_IO.Read (Stream.File, Stream.Current_Character);
    end Next_Char;

    overriding
    procedure Open (Stream : in out File_Backend) is
    begin
        Char_IO.Open (Stream.File,
                      Char_IO.In_File,
                      To_String (Stream.Filename));

        Char_IO.Read (Stream.File, Stream.Current_Character);
    end Open;

    overriding
    procedure Previous_Char (Stream : in out File_Backend) is
    begin
        if Stream.Current_Position = 1 then
            raise Apagerlib.Backend.No_More_Char;
        end if;

        Stream.Set_Position (Stream.Current_Position - 1);
        Char_IO.Read (Stream.File, Stream.Current_Character);
    end Previous_Char;

    procedure Set_Filename (Stream : in out File_Backend; Name : String) is
    begin
        Stream.Filename := To_Unbounded_String (Name);
    end Set_Filename;

    overriding
    procedure Set_Position (Stream : in out File_Backend; Position : Positive)
    is
    begin
        Char_IO.Set_Index (Stream.File, Char_IO.Positive_Count (Position));
    end Set_Position;

end Apagerlib.File_Backend;
