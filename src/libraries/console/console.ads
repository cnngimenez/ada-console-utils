--  console.ads ---

--  Copyright 2019 cnngimenez
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

with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

--  Ada implementation for VT102 and other terminal Controls.
--
--  See manpage console_codes (4) for more information.  Also Wikipedia
--  article https://en.wikipedia.org/wiki/ANSI_escape_code has More
--  explanation and references.
package Console is

    CSI : constant String := ESC & "[";
    OSC : constant String := ESC & "]";
    ST : constant String := ESC  & "\";

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
