-- console.adb --- 

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

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;
with Ada.Text_Io;
use Ada.Text_Io;

package body Console is
    procedure Reset_All is
    begin
        Put (ESC & "[0m");
    end Reset_All;
    
    procedure Bold is 
    begin 
        Put (ESC & "[1m");
    end Bold;
    
    procedure Faint is
    begin
        Put (ESC & "[2m");
    end Faint;
    
    procedure Crossed_Out is
    begin
        Put (ESC & "[9m");
    end Crossed_Out;
    
    procedure Alternative_Font is
    begin
        Put (ESC & "[11m");
    end Alternative_Font;
    
    function Colour_To_Ansi (C : Colour) return String is
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
    
    function Colour_To_Ansi_Bright (C : Colour) return String is
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
    
    function Bg_Colour_To_Ansi (C : Colour) return String is
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
    
    function Bg_Colour_To_Ansi_Bright (C : Colour) return String is
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
    
    
    procedure Set_Colour (C : Colour; Bright : Boolean := False) is
    begin
        if Bright then
            Put (Colour_To_Ansi_Bright (C));
        else
            Put (Colour_To_Ansi (C));
        end if;
    end Set_Colour;
    
    procedure Set_Background (C : Colour; Bright : Boolean := False) is
    begin
        if Bright then
            Put (Bg_Colour_To_Ansi_Bright (C));
        else
            Put (Bg_Colour_To_Ansi (C));
        end if;
    end Set_Background;
    
    procedure Underline is
    begin
        Put (ESC & "[4m");
    end Underline;
    
    procedure Blink is
    begin
        Put (ESC & "[5m");
    end Blink;
    
    procedure Reverse_Video is 
    begin
        Put (ESC & "[7m");
    end Reverse_Video;
       
    procedure Crossed_Out_Off is
    begin
        Put (ESC & "[29m");
    end Crossed_Out_Off;
    
    procedure Primary_Font is
    begin
        Put (ESC & "[10m");
    end Primary_Font;
    
    procedure Colour_Off is
    begin
        Put (ESC & "[39m");
    end Colour_Off;
    
    procedure Background_Off is
    begin
        Put (ESC & "[49m");
    end Background_Off;
    
    procedure Underline_Off is
    begin
        Put (ESC & "[24m");
    end Underline_Off;
    
    procedure Blink_Off is
    begin
        Put (ESC & "[25m");
    end Blink_Off;
    
    procedure Reverse_Video_Off is
    begin
        Put (ESC & "[27m");
    end Reverse_Video_Off;
end Console;
