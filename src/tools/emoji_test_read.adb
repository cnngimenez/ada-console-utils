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

with Ada.Wide_Wide_Text_Io;
with Ada.Text_Io;
use Ada.Text_Io;

with Emojis.List;
use Emojis.List;

procedure Emoji_Test_Read is
    
    package Wwio renames Ada.Wide_Wide_Text_Io;
    
    Emoji_Description : Emoji_Description_Type;
begin
    Put_Line ("Test line?");
    declare 
        Line : String := Get_Line;
    begin
        Parse_Test (Line, Emoji_Description);
        
        Wwio.Put_Line 
          (Emoji_String.To_Wide_Wide_String
             (Emoji_Description.Emoji));
        Wwio.Put_Line 
          (Code_String.To_Wide_Wide_String
             (Emoji_Description.Code));
        
        Put_Line (Emoji_Description.Status'Image);
        Put_Line (Emoji_Description.Version.Major'Image & "." 
                    & Emoji_Description.Version.Minnor'Image);
        Put_Line 
          (Name_String.To_String
           (Emoji_Description.Name));
    end;
end Emoji_Test_Read;
