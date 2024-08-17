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
    --  Open the stream and initialise it.
    --
    --  Requires override.

    function End_Of_File (Stream : Backend_Stream) return Boolean;
    --  Return when the pointer is at end of file.
    --
    --  Requires override.

    function Current_Position (Stream : Backend_Stream) return Positive;
    --  Return the current pointer position of the stream.
    --
    --  Requires override.

    procedure Set_Position (Stream : in out Backend_Stream;
                            Position : Positive);
    --  Change the current position.
    --
    --  Requires override.

    function Get_Char (Stream : in out Backend_Stream) return Character;
    --  Return the current character.
    --
    --  Do not move the cursor.
    --  Requires override.

    function Previous_Char (Stream : in out Backend_Stream'Class)
        return Character;
    --  Move the pointer and return the previous character.
    --
    --  Override is optional.

    procedure Previous_Char (Stream : in out Backend_Stream);
    --  Move the pointer and current character to the previous one.
    --
    --  Raise No_More_Char if there is no previous char (position 1 reached).
    --  Requires override.

    function Next_Char (Stream : in out Backend_Stream'Class)
        return Character;
    --  Override is optional.

    procedure Next_Char (Stream : in out Backend_Stream);
    --  Move the pointer and current character to the next one.
    --
    --  Raise No_More_Char if there is no next char (End of file reached).
    --  Requires override.

    function Next_Line_Position (Stream : in out Backend_Stream'Class;
                                 Start_Position : Positive)
                                 return Positive;
    --  Move the cursor to the next line and return the position.
    --
    --  No_Line_Found exception is raised when there is no next line.
    --  Override is optional.

    function Previous_Line_Position (Stream : in out Backend_Stream'Class;
                                     Start_Position : Positive)
                                     return Positive;
    --  Move the cursor to the previous line and return the position.
    --
    --  No_Line_Found exception is raised when there is no previous line.
    --  Requires override.

    procedure Next_Line (Stream : in out Backend_Stream'Class);
    --  No_Line_Found exception is raised when there is no next line.
    --  Override is optional.

    procedure Previous_Line (Stream : in out Backend_Stream'Class);
    --  No_Line_Found exception is raised when there is no previous line.
    --  Override is optional.

    procedure Beginning_Position (Stream : in out Backend_Stream);
    --  Move the cursor to the first character position.

    procedure End_Position (Stream : in out Backend_Stream);
    --  Move the cursor to the last character position.
    --
    --  It should not set End_Of_File to true.
    --  Requires override.

    function End_Position (Stream : in out Backend_Stream'Class)
        return Positive;
    --  Move the cursor to the end of stream and return the position.
    --
    --  Override is optional.

    procedure Close (Stream : in out Backend_Stream);
    --  Requires override.

    No_More_Char : exception;
    No_Line_Found : exception;

private
    type Backend_Stream is tagged limited
    record
        null;
    end record;

end Apagerlib.Backend;
