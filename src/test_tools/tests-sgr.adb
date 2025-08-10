--  tests-sgr.adb ---

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

with Ada.Text_IO;
use Ada.Text_IO;
with Console.SGR;
use Console.SGR;

package body Tests.SGR is

    procedure Alternative_Font_Test is
    begin
        for Font_Number in Alt_Font_Type'First .. Alt_Font_Type'Last loop
            Alternative_Font (Font_Number);
            Write ("Alternative_Font (" & Font_Number'Image & ")");
        end loop;
    end Alternative_Font_Test;

    procedure Bright_Background_Test is
    begin
        for Colour in Colour_Type'First .. Colour_Type'Last loop
            Set_Background (Colour, True);
            Write ("Set_Background (" & Colour'Image & ", Bright := True);");
        end loop;
    end Bright_Background_Test;

    procedure Bright_Colour_Test is
    begin
        for Colour in Colour_Type'First .. Colour_Type'Last loop
            Set_Colour (Colour, True);
            Write ("Set_Colour (" & Colour'Image & ", Bright := True);");
        end loop;
    end Bright_Colour_Test;

    procedure Set_Background_8bit_Test is
    begin
        for Num in Colour_8bit_Type'First .. Colour_8bit_Type'Last loop
            if Num = 15 then
                Put_Line ("");
            end if;
            if (Num - 15) mod 36 = 0 then
                Put_Line ("");
            end if;

            Set_8bit_Background (Num);
            Put ("A");
            Reset_All;
        end loop;

        New_Line;
    end Set_Background_8bit_Test;

    procedure Set_Background_Test is
    begin
        for Colour in Colour_Type'First .. Colour_Type'Last loop
            Set_Background (Colour);
            Write ("Set_Background (" & Colour'Image & ");");
            Set_Background (Normal);
        end loop;
    end Set_Background_Test;

    procedure Set_Colour_8bit_Test is
    begin
        for Num in Colour_8bit_Type'First .. Colour_8bit_Type'Last loop
            if Num = 15 then
                Put_Line ("");
            end if;
            if (Num - 15) mod 36 = 0 then
                Put_Line ("");
            end if;

            Set_8bit_Colour (Num);
            Put ("A");
            Reset_All;
        end loop;

        New_Line;
    end Set_Colour_8bit_Test;

    procedure Set_Colour_Test is
    begin
        for Colour in Colour_Type'First .. Colour_Type'Last loop
            Set_Colour (Colour);
            Write ("Set_Colour (" & Colour'Image & ");");
        end loop;
    end Set_Colour_Test;

    procedure Set_RGB_Background_Test is
    begin
        for R in RGB_Number_Type'First .. RGB_Number_Type'Last / 20
        loop
            for G in RGB_Number_Type'First .. RGB_Number_Type'Last / 20
            loop
                for B in RGB_Number_Type'First .. RGB_Number_Type'Last / 20
                loop
                    Set_RGB_Background (R * 20, G * 20, B * 20);
                    Put ("A");
                    Reset_All;
                end loop;
                Put (" ");
            end loop;

            New_Line;
        end loop;
    end Set_RGB_Background_Test;

    procedure Set_RGB_Test is
    begin
        for R in RGB_Number_Type'First .. RGB_Number_Type'Last / 20
        loop
            for G in RGB_Number_Type'First .. RGB_Number_Type'Last / 20
            loop
                for B in RGB_Number_Type'First .. RGB_Number_Type'Last / 20
                loop

                    Set_RGB_Colour (R * 20, G * 20, B * 20);
                    Put ("A");
                    Reset_All;

                end loop;
                Put (" ");
            end loop;

            New_Line;

        end loop;
    end Set_RGB_Test;

    procedure Test_Codes_0_To_10 is
    begin
        --  Code 0
        Put_Line ("----> Testing codes from 0 to 9. ");

        Bold;
        Put_Line ("Bold");
        Bold;
        Put_Line ("Bold (twice)");
        Reset_All;

        Faint;
        Put_Line ("Faint");
        Faint;
        Put_Line ("Faint (twice)");
        Reset_All;

        Italic;
        Write ("Italic");

        Underline;
        Write ("Underline");

        Blink;
        Write ("Blink");

        Rapid_Blink;
        Write ("Rapid_Blink");

        Reverse_Video;
        Write ("Reverse");

        Conceal;
        Write ("Conceal");

        Crossed_Out;
        Write ("Crossed");

    end Test_Codes_0_To_10;

    procedure Test_Codes_10_To_20 is
    begin
        --  Code 10
        Put_Line ("----> Testing codes from 10 to 19. ");

        Primary_Font;
        Write ("Primary");

        Alternative_Font_Test;
    end Test_Codes_10_To_20;

    procedure Test_Codes_20_To_30 is
    begin
        --  Code 20
        Put_Line ("----> Testing codes from 20 to 29. ");

        Fraktur;
        Write ("Fraktur");

        Doubly_Underline; --  Also Bold_Off.
        Write ("Doubly_Underline");

        Normal_Colour; --  Also normal intensity
        Write ("Normal_Colour");

        Not_Italic; --  Also Not_Fraktur;
        Write ("Not_Italic");

        Underline_Off;
        Write ("Underline_Off");

        Blink_Off;
        Write ("Blink_Off");

        Reverse_Video_Off; --  Inverse off.
        Write ("Reverse_Video_Off");

        Reveal;
        Write ("Reveal");

        Crossed_Out_Off; --  Not crossed out
        Write ("Crossed_Out_Off");
    end Test_Codes_20_To_30;

    procedure Test_Codes_30_To_40 is
    begin
        --  Code 30
        Put_Line ("----> Testing codes from 30 to 39. ");

        Set_Colour_Test;

        Put_Line ("--> 8 bit colour test:");
        Set_Colour_8bit_Test;
        Reset_All;

        Put_Line ("--> 24 bit colour test: (RGB)");
        Set_RGB_Test;

        Default_Colour;
        Write ("Default_Colour");
    end Test_Codes_30_To_40;

    procedure Test_Codes_40_To_50 is
    begin
        --  Code 40
        Put_Line ("----> Testing codes from 40 to 49.");

        Put_Line ("--> 3/4 bit colour test:");
        Set_Background_Test;

        Put_Line ("--> 8 bit colour test:");
        Set_Background_8bit_Test;
        Reset_All;

        Put_Line ("--> 24 bit colour test: (RGB)");
        Set_RGB_Background_Test;

        Default_Background;
        Write ("Default_Background");
    end Test_Codes_40_To_50;

    procedure Test_Codes_50_To_60 is
    begin
        --  Code 50
        Put_Line ("----> Testing codes from 50 to 59. ");

        Framed;
        Write ("Framed");

        Encircled;
        Write ("Encircled");

        Overlined;
        Write ("Overlined");

        Not_Framed;
        Write ("Not_Framed");

        Not_Overlined;
        Write ("Not_Overlined");
    end Test_Codes_50_To_60;

    procedure Test_Codes_60_To_70 is
    begin
        --  Code 60
        Put_Line ("----> Testing codes from 60 to 69. ");

        Ideogram_Underline;
        Write ("Ideogram_Underline");

        Ideogram_Double_Underline;
        Write ("Ideogram_Double_Underline");

        Ideogram_Overline;
        Write ("Ideogram_Overline");

        Ideogram_Double_Overline;
        Write ("Ideogram_Double_Overline");

        Ideogram_Stress;
        Write ("Ideogram_Stress");

        Ideogram_Off;
        Write ("Ideogram_Off");
    end Test_Codes_60_To_70;

    procedure Test_Codes_90_To_110 is
    begin
        --  Code 90 - 97

        Put_Line ("----> Bright colour test (90-97 codes):");
        Bright_Colour_Test;

        --  Code 100 - 107
        Put_Line ("----> Bright colour test (100-107 codes):");
        Bright_Background_Test;
    end Test_Codes_90_To_110;

    procedure Write (Str : String) is
    begin
        Put_Line (Str);
        Console.SGR.Reset_All;
    end Write;

end Tests.SGR;
