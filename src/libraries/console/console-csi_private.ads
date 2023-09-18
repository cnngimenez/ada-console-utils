--  console-csi_private.ads ---

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
--  DEC Private Sequences
--
--  CSI numbers in this case are "ESC[?" characters.
--
package Console.CSI_Private is

    --  Codes 0:

    procedure Cursor_Keys_Sends_Esc_O (Enable : Boolean := False);
    --  Send ESC O instead ESC [ when cursor key is pressed (CSI 1).
    --
    --  DECCKM.

    type Column_Mode_Type is (Column_80, Column_132);

    procedure Column_Mode_Switch (
        Column_Mode : Column_Mode_Type := Column_80);
    --  80/132 column mode switch (CSI 3).
    --
    --  DECCOLM.

    procedure Reverse_Video (Enable : Boolean := False);
    --  Enable/disable reverse video (CSI 5).
    --
    --  DECSCNM.

    procedure Cursor_Address_Scrollig_Region (Enable : Boolean := False);
    --  Cursor address relative to upper left corner (CSI 6).
    --
    --  DECOM.

    procedure Autowrap (Enable : Boolean := True);
    --  Enable/disable autowrap (CSI 7).
    --
    --  DECAWM.  Characters printed after column 80 would be printed at
    --  the firts column at the next line.

    procedure Autorepeat_Keyboard (Autorepeat : Boolean := True);
    --  Enable/disable keyboard autorepeat (CSI 8)
    --
    --  DECARM.

    --  Codes 20:

    procedure Cursor_Visible (Visible : Boolean := True);
    --  Make the cursor visible (CSI 25).
    --
    --  DECTECM.

    --  CSI 1000: check mouse.ads library!

    --  -------------------------
    --  Aliases
    --  -------------------------

    procedure Hide_Cursor;
    procedure Show_Cursor;

end Console.CSI_Private;
