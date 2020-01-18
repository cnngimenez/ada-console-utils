-- emojis-list.adb --- 

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
with Ada.Characters.Conversions;
with Ada.Wide_Wide_Text_Io;
with Ada.Text_Io;
with Gnat.Regpat;

use Ada.Characters.Conversions;


package body Emojis.List is
    
    Comment_Pattern : String := "^#.*";
    
    Code_Pattern : String := "^([[:digit:]A-F ]+) ";
    Status_Pattern : String := "; (.*) ";
    Emoji_Pattern : String := "# ([^E]+) ";
    Version_Pattern : String := "E([[:digit:]]+)\.([[:digit:]]+) ";
    Description_Pattern : String := "(.*)$";
    
    -- "^\\([[:digit:]A-F ]+\\); \\(.*\\) # \\([^E]+\\)" &
    -- " E\\([[:digit:]]+\\)\.\\([[:digit:]]+\\) \\(.*\\)$"
    Emoji_Description_Pattern : String := 
      Code_Pattern & 
      Status_Pattern & 
      Emoji_Pattern & 
      Version_Pattern & 
      Description_Pattern;
    
    
    function To_Status (Str : String) return Status_Type is
        Ret : Status_Type;
    begin
        if Str = "component" then
            Ret := Component;
        elsif Str = "fully-qualified" then
            Ret := Fully_Qualified;
        elsif Str =  "minimally-qualified" then
            Ret := Minimally_Qualified;
        elsif Str = "unqualified" then
            Ret := Unqualified;
        end if;
        return Ret;
    end To_Status;
    
    function Is_Comment (Str : String) return Boolean is
        use Gnat.Regpat;
        
        Compiled_Expression : Pattern_Matcher := 
          Compile (Comment_Pattern);
        Result : Match_Array (0 .. 1);
    begin
        Match (Compiled_Expression, Str, Result);
        
        if Result (0) /= No_Match then
            return True;
        else
            return False;
        end if;
    end Is_Comment;
     
    procedure Parse_Test (Str : String; 
                          Emoji_Description : out Emoji_Description_Type) is
                
        use Gnat.Regpat;
        use Ada.Strings.Fixed;
        use Ada.Strings;
                
        Compiled_Expression : Pattern_Matcher := 
          Compile(Emoji_Description_Pattern);
        Result : Match_Array (0..6);
        
        procedure Parse_Code is
            Code_Str : String :=               
              Trim (Str (Result (1).First .. Result (1).Last), Both);
            Code_Wws : Wide_Wide_String :=
              To_Wide_Wide_String(Code_Str);
        begin
            -- Put_Line ("Result 1:" & Code_Str);
            Emoji_Description.Code := 
              Code_String.To_Bounded_Wide_Wide_String (Code_Wws, Right);
        end Parse_Code;
        
        procedure Parse_Status is
            Status_Str : String := 
              Str (Result(2).First .. Result(2).Last);
        begin
            --  Put_Line ("Result 2:" & Status_Str);
            Emoji_Description.Status := To_Status (Status_Str);
        end Parse_Status;
        
        procedure Parse_Emoji is
            Emoji_Str : String := 
              Str (Result (3).First .. Result (3).Last);
            Emoji_Wws : Wide_Wide_String := 
              To_Wide_Wide_String (Emoji_Str);
           
        begin
            --  Put_Line ("Result 3:" & Emoji_Str);
            --  Wwio.Put_Line ("Result 3 ww:" & Emoji_Wws);
            
            Emoji_Description.Emoji := 
              Emoji_String.To_Bounded_Wide_Wide_String (Emoji_Wws, Right);
            
        end Parse_Emoji;            
        
        procedure Parse_Major_Version is
            Data_Str : String := 
              Str (Result (4).First .. Result (4).Last);
        begin
            --  Put_Line ("Result 4:" & Data_Str);
            Emoji_Description.Version.Major :=  
              Natural'Value (Data_Str);
        end Parse_Major_Version;
        
        procedure Parse_Minnor_Version is
            Data_Str : String := 
              Str (Result (5).First .. Result (5).Last);
        begin
            --  Put_Line ("Result 5:" & Data_Str);
            Emoji_Description.Version.Minnor := 
              Natural'Value (Data_Str);
        end Parse_Minnor_Version;
        
        procedure Parse_Description is
            Matched_Str : String := 
              Str(Result(6).First .. Result(6).Last);
        begin
            --  Put_Line ("Result 6:" & Matched_Str);
            Emoji_Description.Name := 
              Name_String.To_Bounded_String (Matched_Str, Right);
        end Parse_Description;
                   
    begin
        Match (Compiled_Expression, Str, Result);
        
        if Result (1) /= No_Match then 
            Parse_Code;
        end if;
        
        if Result (2) /= No_Match then
            Parse_Status;
        else
            Emoji_Description.Status := Unknown;
        end if;
        
        if Result(3) /= No_Match then
            Parse_Emoji;
        end if;
        
        if Result(4) /= No_Match then
            Parse_Major_Version;
        end if;
        
        if Result(5) /= No_Match then
            Parse_Minnor_Version;
        end if;
        
        if Result(6) /= No_Match then
            Parse_Description;
        end if;
    end Parse_Test;
    
    procedure Open_Test_File (Path : String; File : out File_Type) is
        use Ada.Text_Io;
    begin
        Open (File, In_File, Path);
    end Open_Test_File;
    
    procedure Next_Test (File : in out File_Type;
                         Emoji_Description : out Emoji_Description_Type) is
        use Ada.Text_Io;
    begin
        if End_Of_File (File) then
            Emoji_Description := Invalid_Emoji_Description;
            return;
        end if;
        
        declare
            use Ada.Strings.Fixed;
            Line : String := Trim (Get_Line (File), Both);
        begin           
            if Is_Comment (Line) or Line = "" then
                Emoji_Description := Invalid_Emoji_Description;
                Emoji_Description.Name := 
                  Name_String.To_Bounded_String (Line, Right);
            else
                Parse_Test (Line, Emoji_Description);
            end if;
        end;
    end Next_Test;
    
    procedure Close_Test_File (File : in out File_Type) is 
    begin
        Close (File);
    end Close_Test_File;
end Emojis.List;
