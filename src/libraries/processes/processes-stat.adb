--  processes-stat.adb ---

--  Copyright 2025 cnngimenez
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

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

package body Processes.Stat is

    procedure Parse_Stat (F : in out File_Type; Stat : out Stat_Type);
    function Next_Field (F : in out File_Type) return String;
    function To_Comm_String (Str : String) return Comm_String;

    function Next_Field (F : in out File_Type) return String is
        use Ada.Strings.Unbounded;

        Buffer : Unbounded_String;
        C : Character := 'A';
    begin
        while not End_Of_File (F) and then C /= ' '
        loop
            Get (F, C);
            if C /= ' ' then
                Append (Buffer, C);
            end if;
        end loop;

        return To_String (Buffer);
    end Next_Field;

    procedure Parse_Stat (F : in out File_Type; Stat : out Stat_Type) is
    begin
        Stat.Pid := PID_Type'Value (Next_Field (F));
        Stat.Comm := To_Comm_String (Next_Field (F));
        Stat.State := State_String_To_Type (Next_Field (F));
        Stat.Ppid := PID_Type'Value (Next_Field (F));
        Stat.Pgrp := Integer'Value (Next_Field (F));
        Stat.Session := Integer'Value (Next_Field (F));
        Stat.Tty_Nr := Integer'Value (Next_Field (F));
        Stat.Tpgid := Integer'Value (Next_Field (F));
        Stat.Flags := Natural'Value (Next_Field (F));
    end Parse_Stat;

    function Read_Stat (Path : String) return Stat_Type is
        File : File_Type;
        Stat : Stat_Type;
    begin
        Open (File, In_File, Path);
        Parse_Stat (File, Stat);
        Close (File);

        return Stat;
    end Read_Stat;

    function Stat_Path (PID : PID_Type) return String
        is ("/proc/"
            & Ada.Strings.Fixed.Trim (PID'Image, Ada.Strings.Both)
            & "/stat");

    function To_Comm_String (Str : String) return Comm_String is
        Comm : Comm_String := "                ";
        I : Natural;
    begin
        I := 1;
        while I < Str'Length and then Str (I + 1) /= ')'
        loop
            Comm (I) := Str (I + 1);
            I := I + 1;
        end loop;

        return Comm;
    end To_Comm_String;

end Processes.Stat;
