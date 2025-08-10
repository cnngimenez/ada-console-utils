--  emoji_test.adb ---

--  Copyright 2019 cnngimenez
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

with Ada.Wide_Wide_Text_IO;
with Ada.Text_IO;
use Ada.Text_IO;
with Emojis;
use Emojis;

with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;

procedure Emoji_Test is

    package Wwio renames Ada.Wide_Wide_Text_IO;

    Unbounded_Str : Unbounded_Wide_Wide_String;

    Face : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#1f600#);
    Person : constant Wide_Wide_Character :=
           Wide_Wide_Character'Val (16#1f468#);
    Hat : constant Wide_Wide_Character := Wide_Wide_Character'Val (16#1f393#);

begin
    Put_Line ("Character A:" & Character'Val (65));

    Put_Person;
    Put_Skin (Dark);
    Put_Hair (Red_Hair);

    Put_Skin (Face, Dark);

    Append (Unbounded_Str, Person);
    Append (Unbounded_Str, Hat);

    Put_Line ("");
    declare
        Joined : constant Unbounded_Wide_Wide_String :=
          Join_With_Zwj (To_Wide_Wide_String (Unbounded_Str));
        Str : constant Wide_Wide_String := (To_Wide_Wide_String (Joined));
    begin
        Put_Line (Natural'Image (Length (Joined)));
        Wwio.Put_Line (Str);
    end;

end Emoji_Test;
