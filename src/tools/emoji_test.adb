-- emoji_test.adb --- 

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

with Ada.Text_Io;
use Ada.Text_Io;
with Emojis;
use Emojis;

procedure Emoji_Test is
begin
    Put_Line ("Character A:" & Character'Val (65));
    
    Put_Person;
    Put_Skin (Dark);
    Put_Hair (Red_Hair);
end Emoji_Test;
