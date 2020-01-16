-- emojis.adb --- 

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
use Ada.Wide_Wide_Text_Io;

package body Emojis is
    function Get_Skin (Skin : Skin_Type) return Wide_Wide_Character is
    begin
        return Wide_Wide_Character'Val 
          (Skin_Type'Pos (Skin) + 16#1F3FB#);
    end Get_Skin;
    
    function Get_Hair (Hair : Hair_Type) return Wide_Wide_Character is
    begin
        return Wide_Wide_Character'Val
          (Hair_Type'Pos (Hair) + 16#1F9B0#);
    end Get_Hair;
    
    
    procedure Put_Skin (Skin : Skin_Type) is
    begin
        Put (Get_Skin (Skin));        
    end Put_Skin;
    
    procedure Put_Hair (Hair : Hair_Type) is
    begin
        Put (Get_Hair (Hair));
    end Put_Hair;
    
    procedure Put_Person is
    begin
        Put (Wide_Wide_Character'Val (16#1F9D1#));
    end Put_Person;
end Emojis;
