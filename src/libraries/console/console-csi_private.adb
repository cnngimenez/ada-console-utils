--  console-csi_private.adb ---

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

--  with Ada.Strings.Fixed;
--  use Ada.Strings.Fixed;
--  use Ada.Strings;
with Ada.Text_IO;
use Ada.Text_IO;

package body Console.CSI_Private is

    procedure Autorepeat_Keyboard (Autorepeat : Boolean := True) is
    begin
        Put (ESC & "[?8"
            & (if Autorepeat then "h" else "l"));
    end Autorepeat_Keyboard;

    procedure Autowrap (Enable : Boolean := True) is
    begin
        Put (ESC & "[?7"
            & (if Enable then "h" else "l"));
    end Autowrap;

    procedure Column_Mode_Switch (
        Column_Mode : Column_Mode_Type := Column_80)
    is
    begin
        Put (ESC & "[?3"
            & (if Column_Mode = Column_80 then "l" else "h"));
    end Column_Mode_Switch;

    procedure Cursor_Address_Scrollig_Region (Enable : Boolean := False)
    is
    begin
        Put (ESC & "[?6"
            & (if Enable then "h" else "l"));
    end Cursor_Address_Scrollig_Region;

    procedure Cursor_Keys_Sends_Esc_O (Enable : Boolean := False) is
    begin
        Put (ESC & "[?1"
            & (if Enable then "h" else "l"));
    end Cursor_Keys_Sends_Esc_O;

    procedure Cursor_Visible (Visible : Boolean := True) is
    begin
        Put (ESC & "[?25"
            & (if Visible then "h" else "l"));
    end Cursor_Visible;

    procedure Hide_Cursor is
    begin
        Cursor_Visible (False);
    end Hide_Cursor;

    procedure Reverse_Video (Enable : Boolean := False) is
    begin
        Put (ESC & "[?5"
            & (if Enable then "h" else "l"));
    end Reverse_Video;

    procedure Show_Cursor is
    begin
        Cursor_Visible (True);
    end Show_Cursor;

end Console.CSI_Private;
