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

    --  -------------------------
    --  C1 (8-bit) Control Characters
    --  -------------------------

    IND : constant String := ESC & "D";
    --  Index.
    NEL : constant String := ESC & "E";
    --  Next Line.
    HTS : constant String := ESC & "H";
    --  Tab Set.
    RI : constant String := ESC & "M";
    --  Reverse Index.
    SS2 : constant String := ESC & "N";
    --  Single Shift Select of G2 Character Set.
    SS3 : constant String := ESC & "O";
    --  Single Shift Select of G3 Character Set.
    DCS : constant String := ESC & "P";
    --  Device Control String.
    SPA : constant String := ESC & "V";
    --  Start of Guarded Area.
    EPA : constant String := ESC & "W";
    --  End of Guarded Area.
    SOS : constant String := ESC & "X";
    --  Start of String.
    DA : constant String := ESC & "Z";
    --  Return Terminal ID.
    --  DA is an obsolete form of CSI.
    CSI : constant String := ESC & "[";
    --  Control Sequence Introducer.
    ST : constant String := ESC  & "\";
    --  String Terminator.
    OSC : constant String := ESC & "]";
    --  Operating System Command.
    PM : constant String := ESC  & "^";
    --  Privacy Message.
    APC : constant String := ESC  & "_";
    --  Application Program Command.

end Console;
