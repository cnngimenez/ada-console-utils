--  console-sgr.ads ---

--  Copyright 2023 cnngimenez
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

--
--  Set Graphic Rendition
--
package Console.SGR is

    --  --------------------------------------------------
    --  CSI Public Sequences - Set Graphic Rendition (SGR)
    --  --------------------------------------------------

    type Colour_Type is (Black, Red, Green, Yellow, Blue,
                         Magenta, Cyan, White, Normal);

    type Alt_Font_Type is range 1 .. 9;

    type RGB_Number_Type is range 0 .. 255;

    type RGB_Colour_Type is record
        Red : RGB_Number_Type;
        Green : RGB_Number_Type;
        Blue : RGB_Number_Type;
    end record;

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
    procedure Set_RGB_Colour (R, G, B : RGB_Number_Type);
    procedure Set_RGB_Colour (Colour : RGB_Colour_Type);
    procedure Set_8bit_Colour (Num : Colour_8bit_Type);
    procedure Default_Colour;  --  Also Colour_Off;

    --  Code 40
    procedure Set_Background (C : Colour_Type; Bright : Boolean := False);
    procedure Set_RGB_Background (R, G, B : RGB_Number_Type);
    procedure Set_RGB_Background (Colour : RGB_Colour_Type);
    procedure Set_8bit_Background (Num : Colour_8bit_Type);
    procedure Default_Background; --  Also Background_Off

    --  Code 50
    procedure Framed;
    procedure Encircled;
    procedure Overlined;
    procedure Not_Framed; --  Also Not_Encircled.
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

    function Bg_Colour_To_Ansi_Bright (C : Colour_Type) return String;
    function Bg_Colour_To_Ansi (C : Colour_Type) return String;
    function Colour_To_Ansi (C : Colour_Type) return String;
    function Colour_To_Ansi_Bright (C : Colour_Type) return String;

end Console.SGR;
