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
    
    procedure Parse_Test (Str : String; 
                          Emoji_Description : out Emoji_Description_Type) is
        
        use Gnat.Regpat;
        use Ada.Strings.Fixed;
        use Ada.Strings;
        
        Compiled_Expression : Pattern_Matcher := 
          Compile(Emoji_Description_Pattern);
        Result : Match_Array (0..6);
        
        use Ada.Text_Io;
    begin
        Match (Compiled_Expression, Str, Result);
        
        if Result(1) /= No_Match then 
            Put_Line (Trim 
                        (Str (Result(1).First .. Result(1).Last), Both)
                     & "|");
            -- Emoji_Description.Code := Emoji_String.To_Bounded_Wide_Wide_String 
            --   (To_Wide_Wide_String
            --     (Trim (Str (Result(1).First .. Result(1).Last), Both))
            --  );
        end if;
        if Result(2) /= No_Match then
            Put_Line ("Hello");
            -- Emoji_Description.Emoji := To_Wide_Wide_String 
            --   (Str (Result(2).First .. Result(1).Last));
        end if;
        if Result(3) /= No_Match then
            Emoji_Description.Status := To_Status 
              (Str (Result(3).First .. Result(3).Last));
        end if;
        if Result(4) /= No_Match then
            Emoji_Description.Version.Major :=  Natural'Value 
              (Str (Result(4).First .. Result(4).Last));
        end if;
        if Result(5) /= No_Match then
            Emoji_Description.Version.Minnor := Natural'Value
              (Str (Result(5).First .. Result(5).Last));
        end if;
        if Result(6) /= No_Match then
            Emoji_Description.Name := 
              Name_String.To_Bounded_String
              (Str(Result(6).First .. Result(6).Last));
        end if;
    end Parse_Test;
    
    procedure Open_Test_File (Path : String; File : out File_Type) Is
    begin
        Open(File, In_File, Path);
    end Open_Test_File;
    
    procedure Next_Test (File : in out File_Type;
                         Emoji_Description : out Emoji_Description_Type) is
    begin
        declare
            Line : String := Get_Line(File);
        begin
            Put_Line("nn");
        end;
    end Next_Test;
    
    procedure Close_Test_File (File : in out File_Type) is 
    begin
        Close(File);
    end Close_Test_File;
end Emojis.List;
