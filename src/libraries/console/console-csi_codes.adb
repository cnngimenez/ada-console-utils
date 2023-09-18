--  console-csi_codes.adb ---

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

package body Console.CSI_Codes is

    procedure Aux_Port_Off is
    begin
        Put (ESC & "[4i");
    end Aux_Port_Off;

    procedure Aux_Port_On is
    begin
        Put (ESC & "[5i");
    end Aux_Port_On;

    procedure Cursor_Back (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "D");
    end Cursor_Back;

    procedure Cursor_Down (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "B");
    end Cursor_Down;

    procedure Cursor_Forward (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "C");
    end Cursor_Forward;

    procedure Cursor_Horizontal (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "G");
    end Cursor_Horizontal;

    procedure Cursor_Next_Line (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "E");
    end Cursor_Next_Line;

    procedure Cursor_Position (Row, Column : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Row'Image, Both)
               & ";" &
               Trim (Column'Image, Both)
               & "H");
    end Cursor_Position;

    procedure Cursor_Previous_Line (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "F");
    end Cursor_Previous_Line;

    procedure Cursor_Up (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "A");
    end Cursor_Up;

    procedure Device_Status_Report is
    begin
        Put (ESC & "[6n");
    end Device_Status_Report;

    procedure Erase_Display (What : Erase_Display_Type) is
    begin
        case What is
        when From_Cursor_To_End =>
            Put (ESC & "[0J");
        when From_Cursor_To_Beginning =>
            Put (ESC & "[1J");
        when Entire_Screen =>
            Put (ESC & "[2J");
        when Entire_Screen_And_Scrollback =>
            Put (ESC & "[3J");
        end case;
    end Erase_Display;

    procedure Erase_Line (What : Erase_Line_Type) is
    begin
        case What is
        when From_Cursor_To_End =>
            Put (ESC & "[0K");
        when From_Cursor_To_Beginning =>
            Put (ESC & "[1K");
        when Entire_Line =>
            Put (ESC & "[2K");
        end case;
    end Erase_Line;

    procedure Restore_Cursor_Position is
    begin
        Put (ESC & "[u");
    end Restore_Cursor_Position;

    procedure Save_Cursor_Position is
    begin
        Put (ESC & "[s");
    end Save_Cursor_Position;

    procedure Scroll_Down (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "T");
    end Scroll_Down;

    procedure Scroll_Up (Steps : Natural := 1) is
    begin
        Put (ESC & "[" &
               Trim (Steps'Image, Both)
               & "S");
    end Scroll_Up;

end Console.CSI_Codes;
