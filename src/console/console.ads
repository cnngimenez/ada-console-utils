-- console.ads --- 

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

package Console is
    type Colour is (Black, Red, Green, Yellow, Blue, 
                    Magenta, Cyan, White, Normal);
    
    procedure Reset_All;
    procedure Bold;
    procedure Faint;
    procedure Crossed_Out;
    procedure Alternative_Font;
    procedure Set_Colour (C : Colour; Bright : Boolean := False);
    procedure Set_Background (C : Colour; Bright : Boolean := False);
    procedure Underline;
    procedure Blink;
    procedure Reverse_Video;
    
    procedure Crossed_Out_Off;
    procedure Primary_Font;
    procedure Colour_Off;
    procedure Background_Off;
    procedure Underline_Off;
    procedure Blink_Off;
    procedure Reverse_Video_Off;
    
end Console;
