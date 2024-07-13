--  apagerlib-display.adb ---

--  Copyright 2024 cnngimenez
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
--  along with this program.  If not, see <http://www.gnu.org/Licenses/>.

-------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Text_IO;
use Ada.Text_IO;
with Console.CSI_Codes;
use Console.CSI_Codes;

package body Apagerlib.Display is

    procedure Show_No_Truncate (Page : Page_Type;
                                Start : Positive := 1;
                                Options : Display_Options);
    procedure Show_Truncate (Page : Page_Type;
                             Start : Positive := 1;
                             Options : Display_Options);

    procedure Show_No_Truncate (Page : Page_Type;
                                Start : Positive := 1;
                                Options : Display_Options) is
        Char_Limit : constant Positive := Options.Columns * Options.Lines;
        I : Positive := Start;
        C : Character;
    begin
        while I <= Char_Limit and then I <= Page_Limit loop
            C := Page (I);
            if Ada.Characters.Handling.Is_Graphic (C) then
                Put (C);
            else
                Put (' ');
            end if;
            I := I + 1;
        end loop;
    end Show_No_Truncate;

    procedure Show_Page (Page : Page_Type;
                         Start : Positive := 1;
                         Options : Display_Options := Default_Display_Options)
    is
    begin
        Erase_Display (Entire_Screen);
        Cursor_Position (1, 1);
        if Options.Truncate then
            Show_Truncate (Page, Start, Options);
        else
            Show_No_Truncate (Page, Start, Options);
        end if;
    end Show_Page;

    procedure Show_Truncate (Page : Page_Type;
                             Start : Positive := 1;
                             Options : Display_Options) is
    begin
        --  To be implemented.
        return;
    end Show_Truncate;

end Apagerlib.Display;