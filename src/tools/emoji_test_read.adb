-- emoji_test_read.adb --- 

-- Copyright 2019 cnngimenez
--
-- Author: cnngimenez

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Wide_Wide_Text_Io;
--  use Ada.Wide_Wide_Text_Io;
with Ada.Text_Io;
use Ada.Text_Io;

with Emojis.List;
use Emojis.List;

procedure Emoji_Test_Read is
    
    package Tio renames Ada.Text_Io;
    
    procedure Print_Help is 
    begin
        Put_Line 
          ("Read from standard input the emoji test text file and" & 
             " parse it. Return the interpreted emoji.");
        Put_Line ("Reads up to the end of file.");
        Put_Line ("");
        Put_Line ("Synopsis:");
        Put_Line ("    bin/emoji_test_read");
        Put_Line ("");
    end Print_Help;
    
    procedure Print_Description (Emoji_Description : Emoji_Description_Type) is
        package Wwio renames Ada.Wide_Wide_Text_Io;    
        
        Pos : Integer;
    begin
        Put ("Emoji:");
        declare 
            Wwstr : Wide_Wide_String := Emoji_String.To_Wide_Wide_String
              (Emoji_Description.Emoji);
            Max : Natural := Emoji_String.Length (Emoji_Description.Emoji);
        begin
            for I in 1 .. Max loop
                Pos := Wide_Wide_Character'Pos (Wwstr (i));
                Tio.Put (Pos'Image);
            end loop;
                        
            Wwio.Put_Line (Wwstr);
        end;
        
        -- Wwio.Put (Emoji_String.To_Wide_Wide_String
        --             (Emoji_Description.Emoji));
        -- Wwio.Put_Line (" ");
        
        Put (" | Code: ");
        Wwio.Put_Line (Code_String.To_Wide_Wide_String
                         (Emoji_Description.Code));
        
        Put ("Status: ");
        Tio.Put_Line (Emoji_Description.Status'Image);
        
        Put ("Version: ");
        Tio.Put_Line (Emoji_Description.Version.Major'Image
                        & "." 
                        & Emoji_Description.Version.Minnor'Image);
        Put ("Description: ");
        Tio.Put_Line 
          (Name_String.To_String
             (Emoji_Description.Name));
    end Print_Description;
    
    procedure Parse_File (Filename : String) is
        File : File_Type;
        Emoji_Description : Emoji_Description_Type;                
    begin
        Open_Test_File (Filename, File);
        
        while not End_Of_File (File) loop
            Put_Line ("Reading line");
            Next_Test (File, Emoji_Description);
            Print_Description (Emoji_Description);
        end loop;
        
        Close_Test_File (File);
    end Parse_File;
    
    procedure Parse_Stdin is
        Emoji_Description : Emoji_Description_Type;
    begin
        --  Put_Line ("Test line?");
        while not End_Of_File loop
            declare 
                Line : String := Get_Line;
            begin
                Parse_Test (Line, Emoji_Description);
                Print_Description (Emoji_Description);
            end;
        end loop;
    end Parse_Stdin;
    
begin
    if Argument_Count > 0 then
        if Argument (1) = "-h" or Argument (1) = "--help" then
            Print_Help;
        else
            Put_Line ("Parsing file mode");
            Parse_File (Argument (1));
            Put_Line ("Done");
        end if;
        
    else
        
        Put_Line ("Parsing STDIN mode");
        Parse_Stdin;
        Put_Line ("Done");
    
    end if;
end Emoji_Test_Read;
