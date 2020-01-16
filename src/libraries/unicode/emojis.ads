-- emojis.ads --- 

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

package Emojis is
    
    -- Emoji components
    -- Regional_Indicator is ...
    type Skin_Type is (Light, Medium_Light, Medium, Medium_Dark, Dark);
    type Hair_Type is (Red_Hair, Curly_Hair, Bald, White_Hair);
    
    Zero_Width_Joiner : constant Wide_Wide_Character := 
      Wide_Wide_Character'Val(16#200D#);
    Combining_Enclosing_Keycap : constant Wide_Wide_Character := 
      Wide_Wide_Character'Val(16#20E3#);
    Variation_Selector : constant Wide_Wide_Character := 
      Wide_Wide_Character'Val(16#FE0f#);
    
    function Get_Skin (Skin : Skin_Type) return Wide_Wide_Character;
    function Get_Hair (Hair : Hair_Type) return Wide_Wide_Character;
    
    procedure Put_Skin (Skin : Skin_Type);
    procedure Put_Hair (Hair : Hair_Type);
    
    procedure Put_Person;
    
end Emojis;
