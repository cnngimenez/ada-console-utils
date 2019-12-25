-- console_test.adb --- 

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

with Console;
use Console;

procedure Console_Test is
    
    procedure Write (Str : String) is
    begin
        Put_Line (Str);
        Reset_All;
    end Write;
    
begin
    Bold;
    Write ("Bold");
    
    Faint;
    Write ("Faint");
    
    Crossed_Out;
    Write ("Crossed Out");
    
    Alternative_Font;
    Write ("Alternative Font");
    
    Set_Colour (Black);
    Write ("Set_Colour (Black);");    
    Set_Colour (Red);
    Write ("Set_Colour (Red);");
    Set_Colour (Green);
    Write ("Set_Colour (Green);");
    Set_Colour (Yellow);
    Write ("Set_Colour (Yellow);");
    Set_Colour (Blue);
    Write ("Set_Colour (Blue);");
    Set_Colour (Magenta);
    Write ("Set_Colour (Magenta);");
    Set_Colour (Cyan);
    Write ("Set_Colour (Cyan);");
    Set_Colour (White);
    Write ("Set_Colour (White);");
    Set_Colour (Normal);
    Write ("Set_Colour (Normal);");
                                                                               
    Set_Background (Black);
    Write ("Set_Background (Black);");    
    Set_Background (Red);
    Write ("Set_Background (Red);");
    Set_Background (Green);
    Write ("Set_Background (Green);");
    Set_Background (Yellow);
    Write ("Set_Background (Yellow);");
    Set_Background (Blue);
    Write ("Set_Background (Blue);");
    Set_Background (Magenta);
    Write ("Set_Background (Magenta);");
    Set_Background (Cyan);
    Write ("Set_Background (Cyan);");
    Set_Background (White);
    Write ("Set_Background (White);");
    Set_Background (Normal);
    Write ("Set_Background (Normal);");
    
    Underline;
    Write ("Underline;");
    
    Blink;
    Write ("Blink;");
    
    Reverse_Video;
    Write ("Reverse_Video;");
    
end Console_Test;
