--  apagerlib-backend.ads ---

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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

package Apagerlib.Backend is

    type Backend_Stream is tagged limited private;

    procedure Open (Stream : in out Backend_Stream);

    function End_Of_File (Stream : Backend_Stream) return Boolean;

    function Current_Position (Stream : Backend_Stream) return Positive;

    procedure Set_Position (Stream : in out Backend_Stream;
                            Position : Positive);

    function Get_Char (Stream : in out Backend_Stream) return Character;
    --  Return the current character.
    --
    --  Do not move the cursor.

    function Previous_Char (Stream : in out Backend_Stream'Class)
        return Character;

    procedure Previous_Char (Stream : in out Backend_Stream);

    function Previous_Line_Position (Stream : in out Backend_Stream;
                                     Start_Position : Positive)
                                     return Positive;
    --  Return the previous line position.
    --
    --  Do not move the cursor.

    function Next_Char (Stream : in out Backend_Stream'Class)
        return Character;

    procedure Next_Char (Stream : in out Backend_Stream);

    function Next_Line_Position (Stream : in out Backend_Stream;
                                 Start_Position : Positive)
                                 return Positive;
    --  Return the next line position.
    --
    --  Do not move the cursor.

    procedure Next_Line (Stream : in out Backend_Stream);

    procedure Previous_Line (Stream : in out Backend_Stream);

    procedure Beginning_Position (Stream : in out Backend_Stream);
    --  Move the cursor to the first character position.

    procedure End_Position (Stream : in out Backend_Stream);
    --  Move the cursor to the last character position.
    --
    --  It should not set End_Of_File to true.

    function End_Position (Stream : in out Backend_Stream) return Positive;
    --  Return the end position, the last character position.
    --
    --  It does not move the cursor. It should not set End_Of_File to true.

    procedure Close (Stream : in out Backend_Stream);

    No_More_Char : exception;

private
    type Backend_Stream is tagged limited
    record
        null;
    end record;

end Apagerlib.Backend;
