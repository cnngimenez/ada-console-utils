-- emojis-list.ads --- 

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

with Ada.Strings.Wide_Wide_Bounded;
with Ada.Strings.Bounded;
with Ada.Text_Io;
use Ada.Strings;
use Ada.Text_Io;

package Emojis.List is
    type Status_Type is (Component, Fully_Qualified, 
                         Minimally_Qualified, Unqualified,
                         Unknown);
    
    package Code_String is new 
      Wide_Wide_Bounded.Generic_Bounded_Length (Max => 43);
    package Emoji_String is new 
      Wide_Wide_Bounded.Generic_Bounded_Length (Max => 10);
    package Name_String is new
      Bounded.Generic_Bounded_Length (Max => 100);
    
    type Version_Type is tagged record
        Major: Natural;
        Minnor: Natural;
    end record;
    
    type Emoji_Description_Type is tagged record
        Code : Code_String.Bounded_Wide_Wide_String;
        Emoji: Emoji_String.Bounded_Wide_Wide_String;
        Status : Status_Type;
        Version: Version_Type;
        Name : Name_String.Bounded_String;
    end record;
    
    procedure Parse_Test (Str : String; 
                          Emoji_Description : out Emoji_Description_Type);
    
    procedure Open_Test_File (Path : String; File : out File_Type);
    procedure Next_Test (File : in out File_Type; 
                         Emoji_Description : out Emoji_Description_Type);
    procedure Close_Test_File (File : in out File_Type);
    
    
end Emojis.List;
