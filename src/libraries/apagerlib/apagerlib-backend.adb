--  apagerlib-backend.adb ---

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

package body Apagerlib.Backend is

    procedure Beginning_Position (Stream : in out Backend_Stream)
        is null;

    procedure Close (Stream : in out Backend_Stream)
        is null;

    function Current_Position (Stream : Backend_Stream) return Positive
        is (1);

    function End_Of_File (Stream : Backend_Stream) return Boolean
        is (True);

    procedure End_Position (Stream : in out Backend_Stream)
        is null;

    function End_Position (Stream : in out Backend_Stream) return Positive is
    begin
        Stream.End_Position;
        return Stream.Current_Position;
    end End_Position;

    function Get_Char (Stream : in out Backend_Stream) return Character
        is (Character'Val (0));

    function Next_Char (Stream : in out Backend_Stream'Class)
        return Character is
    begin
        Stream.Next_Char;
        return Stream.Get_Char;
    end Next_Char;

    procedure Next_Char (Stream : in out Backend_Stream)
        is null;

    procedure Next_Line (Stream : in out Backend_Stream) is
        use Ada.Characters.Latin_1;

        C : Character := ' ';
    begin
        while C /= LF and then C /= CR loop
            C := Stream.Next_Char;
        end loop;

        exception
        when No_More_Char => raise No_Line_Found;
    end Next_Line;

    function Next_Line_Position (Stream : in out Backend_Stream;
                                 Start_Position : Positive)
                                 return Positive is
    begin
        Stream.Set_Position (Start_Position);
        Stream.Next_Line;

        return Stream.Current_Position;
    end Next_Line_Position;

    procedure Open (Stream : in out Backend_Stream)
        is null;

    function Previous_Char (Stream : in out Backend_Stream'Class)
        return Character is
    begin
        Stream.Previous_Char;
        return Stream.Get_Char;
    end Previous_Char;

    procedure Previous_Char (Stream : in out Backend_Stream)
        is null;

    procedure Previous_Line (Stream : in out Backend_Stream) is
        use Ada.Characters.Latin_1;

        C : Character := ' ';
    begin
        while C /= LF and then C /= CR loop
            C := Stream.Previous_Char;
        end loop;

        exception
        when No_More_Char => raise No_Line_Found;
    end Previous_Line;

    function Previous_Line_Position (Stream : in out Backend_Stream;
                                     Start_Position : Positive)
                                     return Positive is
    begin
        Stream.Set_Position (Start_Position);
        Stream.Previous_Line;

        return Stream.Current_Position;
    end Previous_Line_Position;

    procedure Set_Position (Stream : in out Backend_Stream;
                            Position : Positive)
        is null;

end Apagerlib.Backend;
