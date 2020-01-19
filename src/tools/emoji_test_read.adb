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

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Strings;
use Ada.Strings;

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
    
    --
    --  Print the description of the Emoji_Description_Type variable.
    --
    procedure Print_Description (Emoji_Description : Emoji_Description_Type) is
        package Wwio renames Ada.Wide_Wide_Text_Io;
        
        --
        --  Print the characters bytes that the emoji is composed.
        --
        procedure Print_Characters is
            Wwstr : Wide_Wide_String := Emoji_String.To_Wide_Wide_String
              (Emoji_Description.Emoji);
            Pos : Natural;    
        begin
            for I in Wwstr'Range loop
                Pos := Wide_Wide_Character'Pos (Wwstr (i));
                Put (Trim(Pos'Image, Left));
                Put (" ");                
            end loop;
        end Print_Characters;

    begin
        Put ("Emoji:       ");
        Wwio.Put (Emoji_String.To_Wide_Wide_String
                    (Emoji_Description.Emoji));
        Put_Line ("");
        
        Put ("Characters:  ");
        Print_Characters;
        Put_Line ("");
        
        Put ("Code:        ");
        Wwio.Put_Line (Code_String.To_Wide_Wide_String
                         (Emoji_Description.Code));
        
        Put ("Status:      ");
        Tio.Put_Line (Emoji_Description.Status'Image);
        
        Put ("Version:     ");
        Tio.Put_Line (Trim (Emoji_Description.Version.Major'Image, Left) & 
                        "." &
                        Trim (Emoji_Description.Version.Minnor'Image, Left));
        
        Put ("Description: ");
        Tio.Put_Line 
          (Name_String.To_String
             (Emoji_Description.Name));
    end Print_Description;
    
    procedure Parse_File (Filename : String ; Name_Filter : String := "") is
        File : File_Type;
        Emoji_Description : Emoji_Description_Type;                
    begin
        Open_Test_File (Filename, File);
        
        if Name_Filter /= "" then
            while not End_Of_File (File) loop
                Next_Test (File, Emoji_Description);
                if Name_String.Index 
                  (Emoji_Description.Name, Name_Filter) > 0 then
                    Put_Line ("----------------------------------------");                    
                    Print_Description (Emoji_Description);
                end if;
            end loop;
        else
            while not End_Of_File (File) loop
                Put_Line ("----------------------------------------");
                Next_Test (File, Emoji_Description);
                Print_Description (Emoji_Description);
            end loop;
        end if;
        
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
            
            if Argument_Count > 1 then                
                Parse_File (Argument (1), Argument (2));
            else
                Parse_File (Argument (1));
            end if;
            
            Put_Line ("Done");
        end if;
        
    else
        
        Put_Line ("Parsing STDIN mode");
        Parse_Stdin;
        Put_Line ("Done");
    
    end if;
end Emoji_Test_Read;
