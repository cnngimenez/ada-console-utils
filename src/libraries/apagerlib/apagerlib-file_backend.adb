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

with Ada.Characters.Latin_1;

package body Apagerlib.File_Backend is

    overriding
    procedure Close (Stream : in out File_Backend) is
    begin
        Char_IO.Close (Stream.File);
    end Close;

    overriding
    function Current_Position (Stream : File_Backend) return Positive
        is (Positive (Char_IO.Index (Stream.File)));

    overriding
    function End_Of_File (Stream : File_Backend) return Boolean is
    begin
        return Char_IO.End_Of_File (Stream.File);
    end End_Of_File;

    overriding
    function Get_Char (Stream : in out File_Backend) return Character
        is (Stream.Current_Character);

    overriding
    procedure Next_Char (Stream : in out File_Backend) is
        C : Character;
    begin
        if Char_IO.End_Of_File (Stream.File) then
            Stream.Current_Character := Character'Val (0);
            raise Apagerlib.Backend.No_More_Char;
        else
            Char_IO.Read (Stream.File, C);
            Stream.Current_Character := C;
        end if;
    end Next_Char;

    overriding
    procedure Next_Line (Stream : in out File_Backend) is
        use Ada.Characters.Latin_1;

        C : Character := ' ';
    begin
        while C /= LF and then C /= CR loop
            C := Stream.Next_Char;
        end loop;

        exception
        when No_More_Char => raise No_Line_Found;

    end Next_Line;

    overriding
    function Next_Line_Position (Stream : in out File_Backend;
                                 Start_Position : Positive)
                                 return Positive is
        Line_Position, Last_Position : Positive;
    begin
        Last_Position := Stream.Current_Position;
        Stream.Next_Line;
        Line_Position := Stream.Current_Position;

        Stream.Set_Position (Last_Position);
        return Line_Position;
    end Next_Line_Position;

    overriding
    procedure Open (Stream : in out File_Backend) is
    begin
        Char_IO.Open (Stream.File,
                      Char_IO.In_File,
                      To_String (Stream.Filename));
        Stream.Next_Char;
    end Open;

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
