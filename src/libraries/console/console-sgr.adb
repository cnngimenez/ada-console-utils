--  console-sgr.adb ---

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

with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
use Ada.Strings;
with Ada.Text_IO;
use Ada.Text_IO;

package body Console.SGR is

    procedure Alternative_Font (Font_Number : Alt_Font_Type) is
        Param_Number : Positive := 10;
    begin
        Param_Number := Param_Number + Alt_Font_Type'Pos (Font_Number);
        Put (ESC & "[" &
               Trim (Param_Number'Image, Both)
               & "m");
    end Alternative_Font;

    function Bg_Colour_To_Ansi (C : Colour_Type) return String is
    begin
        case C is
        when Black =>
            return ESC & "[40m";
        when Red =>
            return ESC & "[41m";
        when Green =>
            return ESC & "[42m";
        when Yellow =>
            return ESC & "[43m";
        when Blue =>
            return ESC & "[44m";
        when Magenta =>
            return ESC & "[45m";
        when Cyan =>
            return ESC & "[46m";
        when White =>
            return ESC & "[47m";
        when others =>
            return ESC & "[0m";
        end case;
    end Bg_Colour_To_Ansi;

    function Bg_Colour_To_Ansi_Bright (C : Colour_Type) return String is
    begin
        case C is
        when Black =>
            return ESC & "[100m";
        when Red =>
            return ESC & "[101m";
        when Green =>
            return ESC & "[102m";
        when Yellow =>
            return ESC & "[103m";
        when Blue =>
            return ESC & "[104m";
        when Magenta =>
            return ESC & "[105m";
        when Cyan =>
            return ESC & "[106m";
        when White =>
            return ESC & "[107m";
        when others =>
            return ESC & "[0m";
        end case;
    end Bg_Colour_To_Ansi_Bright;

    procedure Blink is
    begin
        Put (ESC & "[5m");
    end Blink;

    procedure Blink_Off is
    begin
        Put (ESC & "[25m");
    end Blink_Off;

    procedure Bold is
    begin
        Put (ESC & "[1m");
    end Bold;

    function Colour_To_Ansi (C : Colour_Type) return String is
    begin
        case C is
        when Black =>
            return ESC & "[30m";
        when Red =>
            return ESC & "[31m";
        when Green =>
            return ESC & "[32m";
        when Yellow =>
            return ESC & "[33m";
        when Blue =>
            return ESC & "[34m";
        when Magenta =>
            return ESC & "[35m";
        when Cyan =>
            return ESC & "[36m";
        when White =>
            return ESC & "[37m";
        when others =>
            return ESC & "[0m";
        end case;
    end Colour_To_Ansi;

    function Colour_To_Ansi_Bright (C : Colour_Type) return String is
    begin
        case C is
        when Black =>
            return ESC & "[90m";
        when Red =>
            return ESC & "[91m";
        when Green =>
            return ESC & "[92m";
        when Yellow =>
            return ESC & "[93m";
        when Blue =>
            return ESC & "[94m";
        when Magenta =>
            return ESC & "[95m";
        when Cyan =>
            return ESC & "[96m";
        when White =>
            return ESC & "[97m";
        when others =>
            return ESC & "[0m";
        end case;
    end Colour_To_Ansi_Bright;

    procedure Conceal is
    begin
        Put (ESC & "[8m");
    end Conceal;

    procedure Crossed_Out is
    begin
        Put (ESC & "[9m");
    end Crossed_Out;

    procedure Crossed_Out_Off is
    begin
        Put (ESC & "[29m");
    end Crossed_Out_Off;

    procedure Default_Background is
    begin
        Put (ESC & "[49m");
    end Default_Background;

    procedure Default_Colour is
    begin
        Put (ESC & "[39m");
    end Default_Colour;

    procedure Doubly_Underline is
    begin
        Put (ESC & "[21m");
    end Doubly_Underline;

    procedure Encircled is
    begin
        Put (ESC & "[52m");
    end Encircled;

    procedure Faint is
    begin
        Put (ESC & "[2m");
    end Faint;

    procedure Fraktur is
    begin
        Put (ESC & "[20m");
    end Fraktur;

    procedure Framed is
    begin
        Put (ESC & "[51m");
    end Framed;

    procedure Ideogram_Double_Overline is
    begin
        Put (ESC & "[63m");
    end Ideogram_Double_Overline;

    procedure Ideogram_Double_Underline is
    begin
        Put (ESC & "[61m");
    end Ideogram_Double_Underline;

    procedure Ideogram_Off is
    begin
        Put (ESC & "[65m");
    end Ideogram_Off;

    procedure Ideogram_Overline is
    begin
        Put (ESC & "[62m");
    end Ideogram_Overline;

    procedure Ideogram_Stress is
    begin
        Put (ESC & "[64m");
    end Ideogram_Stress;

    procedure Ideogram_Underline is
    begin
        Put (ESC & "[60m");
    end Ideogram_Underline;

    procedure Italic is
    begin
        Put (ESC & "[3m");
    end Italic;

    procedure Normal_Colour is
    begin
        Put (ESC & "[22m");
    end Normal_Colour;

    procedure Not_Framed is
    begin
        Put (ESC & "[54m");
    end Not_Framed;

    procedure Not_Italic is
    begin
        Put (ESC & "[23m");
    end Not_Italic;

    procedure Not_Overlined is
    begin
        Put (ESC & "[55m");
    end Not_Overlined;

    procedure Overlined is
    begin
        Put (ESC & "[53m");
    end Overlined;

    procedure Primary_Font is
    begin
        Put (ESC & "[10m");
    end Primary_Font;

    procedure Put_SGR (Code : Code_Type; Parameter : String := "") is
    begin
        Put (ESC & "[" &
               Trim (Code'Image, Both)
               & Parameter & "m");
    end Put_SGR;

    procedure Rapid_Blink is
    begin
        Put (ESC & "[6m");
    end Rapid_Blink;

    procedure Reset_All is
    begin
        Put (ESC & "[0m");
    end Reset_All;

    procedure Reveal is
    begin
        Put (ESC & "[28m");
    end Reveal;

    procedure Reverse_Video is
    begin
        Put (ESC & "[7m");
    end Reverse_Video;

    procedure Reverse_Video_Off is
    begin
        Put (ESC & "[27m");
    end Reverse_Video_Off;

    procedure Set_8bit_Background (Num : Colour_8bit_Type) is
    begin
        Put (ESC & "[48;5;"
               & Trim (Num'Image, Both)
               & "m");
    end Set_8bit_Background;

    procedure Set_8bit_Colour (Num : Colour_8bit_Type) is
    begin
        Put (ESC & "[38;5;"
               & Trim (Num'Image, Both)
               & "m");
    end Set_8bit_Colour;

    procedure Set_Background (C : Colour_Type; Bright : Boolean := False) is
    begin
        if Bright then
            Put (Bg_Colour_To_Ansi_Bright (C));
        else
            Put (Bg_Colour_To_Ansi (C));
        end if;
    end Set_Background;

    procedure Set_Bright_Background (Colour : Colour_Type) is
    begin
        Set_Background (Colour, True);
    end Set_Bright_Background;

    procedure Set_Bright_Colour (Colour : Colour_Type) is
    begin
        Set_Colour (Colour, True);
    end Set_Bright_Colour;

    procedure Set_Colour (C : Colour_Type; Bright : Boolean := False) is
    begin
        if Bright then
            Put (Colour_To_Ansi_Bright (C));
        else
            Put (Colour_To_Ansi (C));
        end if;
    end Set_Colour;

    procedure Set_RGB_Background (R, G, B : RGB_Number_Type) is
    begin
        Put (ESC & "[48;2;"
               & Trim (R'Image, Both) & ";"
               & Trim (G'Image, Both) & ";"
               & Trim (B'Image, Both) & "m");
    end Set_RGB_Background;

    procedure Set_RGB_Background (Colour : RGB_Colour_Type) is
    begin
        Set_RGB_Background (Colour.Red, Colour.Green, Colour.Blue);
    end Set_RGB_Background;

    procedure Set_RGB_Colour (R, G, B : RGB_Number_Type) is
    begin
        Put (ESC & "[38;2;"
               & Trim (R'Image, Both) & ";"
               & Trim (G'Image, Both) & ";"
               & Trim (B'Image, Both) & "m");
    end Set_RGB_Colour;

    procedure Set_RGB_Colour (Colour : RGB_Colour_Type) is
    begin
        Set_RGB_Colour (Colour.Red, Colour.Green, Colour.Blue);
    end Set_RGB_Colour;

    procedure Underline is
    begin
        Put (ESC & "[4m");
    end Underline;

    procedure Underline_Off is
    begin
        Put (ESC & "[24m");
    end Underline_Off;

end Console.SGR;
