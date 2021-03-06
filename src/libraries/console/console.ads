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
    type Colour_Type is (Black, Red, Green, Yellow, Blue, 
                    Magenta, Cyan, White, Normal);
    
    type Alt_Font_Type is range 1 .. 9;
    
    type RGB_Type is range 0 .. 255;
    type Colour_8bit_Type is range 0 .. 255;
    
    --  Code 0
    procedure Reset_All;
    procedure Bold;
    procedure Faint;    
    procedure Italic;
    procedure Underline;
    procedure Blink;
    procedure Rapid_Blink;
    procedure Reverse_Video;
    procedure Conceal;
    procedure Crossed_Out;
    
    --  Code 10
    procedure Primary_Font;
    procedure Alternative_Font (Font_Number : Alt_Font_Type);
    
    --  Code 20
    procedure Fraktur;
    procedure Doubly_Underline; --  Also Bold_Off.
    procedure Normal_Colour; --  Also normal intensity
    procedure Not_Italic; --  Also Not_Fraktur;
    procedure Underline_Off;
    procedure Blink_Off;
    procedure Reverse_Video_Off; --  Inverse off.
    procedure Reveal;
    procedure Crossed_Out_Off; --  Not crossed out
    
    --  Code 30
    procedure Set_Colour (C : Colour_Type; Bright : Boolean := False);
    procedure Set_RGB_Colour (R, G, B : RGB_Type);
    procedure Set_8bit_Colour (Num : Colour_8bit_Type);
    procedure Default_Colour;  --  Also Colour_Off;
    
    --  Code 40
    procedure Set_Background (C : Colour_Type; Bright : Boolean := False);
    procedure Set_RGB_Background (R, G, B : RGB_Type);
    procedure Set_8bit_Background (Num : Colour_8bit_Type);
    procedure Default_Background; --  Also Background_Off
    
    --  Code 50
    procedure Framed;
    procedure Encircled;
    procedure Overlined;
    procedure Not_Framed; -- Also Not_Encircled.
    procedure Not_Overlined;
    
    --  Code 60
    procedure Ideogram_Underline;
    procedure Ideogram_Double_Underline;
    procedure Ideogram_Overline;
    procedure Ideogram_Double_Overline;
    procedure Ideogram_Stress;
    procedure Ideogram_Off;
    
    --  Code 90
    procedure Set_Bright_Colour (Colour : Colour_Type);
    procedure Set_Bright_Background (Colour : Colour_Type);
    
    --  Aliases
    
    procedure Bold_Off renames Doubly_Underline;
    procedure Normal_Intensity renames Normal_Colour;
    procedure Not_Fraktur renames Not_Italic;
    procedure Inverse_Off renames Reverse_Video_Off;
    procedure Not_Crossed_Out renames Crossed_Out_Off;
    procedure Colour_Off renames Default_Colour;
    procedure Background_Off renames Default_Background;
    procedure Not_Encircled renames Not_Framed;

    
    type Code_Type is range 0 .. 255;
    
    procedure Put_SGR (Code : Code_Type;
                       Parameter : String := "");
    
    procedure Cursor_Up (Steps : Natural := 1);
    procedure Cursor_Down (Steps : Natural := 1);
    procedure Cursor_Forward (Steps : Natural := 1);
    procedure Cursor_Back (Steps : Natural := 1);
    
    procedure Cursor_Next_Line (Steps : Natural := 1);
    procedure Cursor_Previous_Line (Steps : Natural := 1);
    
    procedure Cursor_Horizontal (Steps : Natural := 1);
    
    procedure Cursor_Position (Row, Column : Natural := 1);
    
    type Erase_Display_Type is (From_Cursor_To_End,
                                From_Cursor_To_Beginning,
                                Entire_Screen,
                                Entire_Screen_And_Scrollback);
    procedure Erase_Display (What : Erase_Display_Type);
    
    type Erase_Line_Type is (From_Cursor_To_End,
                             From_Cursor_To_Beginning,
                             Entire_Line);
    procedure Erase_Line (What : Erase_Line_Type);
    
    procedure Scroll_Up (Steps : Natural := 1);
    procedure Scroll_Down (Steps : Natural := 1);
    
    procedure Aux_Port_On;
    procedure Aux_Port_Off;
    
    procedure Device_Status_Report;
    procedure Save_Cursor_Position;
    procedure Restore_Cursor_Position;
    
end Console;
