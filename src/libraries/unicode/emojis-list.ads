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
use Ada.Strings;
with Ada.Text_Io;
use Ada.Text_Io;

--
--  Parse the emoji test file provided by the unicode consortium.
--
--  See: 
--  https://unicode.org/Public/emoji/12.1/emoji-test.txt
--
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
    
    --  An emoji version is divided by two elements: major and minor.
    --  For example E12.1.
    type Version_Type is tagged record
        Major: Natural;
        Minnor: Natural;
    end record;
    
    --  Definition of the Emoji Discription on a test file.
    type Emoji_Description_Type is tagged record
        Code : Code_String.Bounded_Wide_Wide_String;
        Emoji: Emoji_String.Bounded_Wide_Wide_String;
        Status : Status_Type;
        Version: Version_Type;
        Name : Name_String.Bounded_String;
    end record;
    
    --  Parse a line of test file.
    procedure Parse_Test (Str : String; 
                          Emoji_Description : out Emoji_Description_Type);
    
    --  Open a test file.
    procedure Open_Test_File (Path : String; File : out File_Type);
    
    --  Get the next test line and parse it.
    procedure Next_Test (File : in out File_Type; 
                         Emoji_Description : out Emoji_Description_Type);
    
    --  Close the test file.
    procedure Close_Test_File (File : in out File_Type);
    
    --  An invalid emoji constant.
    Invalid_Emoji_Description : constant Emoji_Description_Type := 
      ( Code => Code_String.To_Bounded_Wide_Wide_String (""),
        Emoji => Emoji_String.To_Bounded_Wide_Wide_String (""),
        Status => Unknown,
        Version => (Major => 0,
                   Minnor => 0),
        Name => Name_String.To_Bounded_String ("")
      );

    
end Emojis.List;
