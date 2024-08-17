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
    procedure Next_Character_Test;
    procedure Previous_Character_Test;
    procedure Print_Next_Line;
    procedure Print_Next_Line_Function;
    procedure Print_Previous_Line;
    procedure Print_Previous_Line_Function;

    Memory : access Backend_Stream'Class;
    File1 : aliased File_Backend;
    Stdin : aliased Page_Memory;

    procedure Print_Next_Line is
    begin
        Memory.Open;

        while not Memory.End_Of_File loop
            Memory.Next_Line;
            Put (Memory.Current_Position'Image);
        end loop;

        Memory.Close;
    end Print_Next_Line;

    procedure Print_Next_Line_Function is
        Position : Positive;
    begin
        Memory.Open;

        while not Memory.End_Of_File loop
            Position := Memory.Current_Position;
            Put (Memory.Next_Line_Position (Position)'Image);
        end loop;

        Memory.Close;
    end Print_Next_Line_Function;

    procedure Print_Previous_Line is
    begin
        Memory.Open;
        Memory.End_Position;

        while Memory.Current_Position > 1 loop
            Memory.Previous_Line;
            Put (Memory.Current_Position'Image);
        end loop;

        Memory.Close;
    end Print_Previous_Line;

    procedure Print_Previous_Line_Function is
        Position : Positive;
    begin
        Memory.Open;
        Memory.End_Position;

        while Memory.Current_Position >= 1 loop
            Position := Memory.Current_Position;
            Put (Memory.Previous_Line_Position (Position)'Image);
        end loop;

        Memory.Close;
    end Print_Previous_Line_Function;

    procedure Next_Character_Test is
    begin
        Memory.Open;

        Put (Memory.Get_Char);
        while not Memory.End_Of_File loop
            Put (Memory.Next_Char);
        end loop;

        Memory.Close;
    end Next_Character_Test;

    procedure Previous_Character_Test is
    begin
        Memory.Open;
        Memory.End_Position;

        Put (Memory.Get_Char);
        while Memory.Current_Position > 1 loop
            Put (Memory.Previous_Char);
        end loop;

        Memory.Close;
    end Previous_Character_Test;

begin
    if Argument_Count > 0 then
        Put_Line ("Using File_Backend");
        File1.Set_Filename (Argument (1));
        Memory := File1'Access;
    else
        Put_Line ("Using Page_Memory");
        Memory := Stdin'Access;
    end if;

    Put_Line ("Print Next_Charecter stream:");
    Next_Character_Test;
    New_Line;

    Put_Line ("Print Previous_Charecter stream:");
    Previous_Character_Test;
    New_Line;

    Put_Line ("Print next line positions:");
    Print_Next_Line;
    New_Line;

    Put_Line ("Print next line positions (with function):");
    Print_Next_Line_Function;
    New_Line;

    Put_Line ("Print previous line positions:");
    Print_Previous_Line;
    New_Line;

    --  Put_Line ("Print previous line positions (with function):");
    --  Print_Previous_Line_Function;
    --  New_Line;
end Apager_Backend_Test;
