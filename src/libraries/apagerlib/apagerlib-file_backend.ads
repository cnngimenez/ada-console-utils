--  apagerlib-file_backend.ads ---

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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Direct_IO;

with Apagerlib.Backend;
use Apagerlib.Backend;

package Apagerlib.File_Backend is

    type File_Backend is new Backend_Stream with private;

    procedure Set_Filename (Stream : in out File_Backend; Name : String);

    overriding
    procedure Open (Stream : in out File_Backend);

    overriding
    function End_Of_File (Stream : File_Backend) return Boolean;

    overriding
    function Current_Position (Stream : File_Backend) return Positive;

    overriding
    procedure Set_Position (Stream : in out File_Backend; Position : Positive);

    overriding
    function Get_Char (Stream : in out File_Backend) return Character;

    overriding
    function Next_Line_Position (Stream : in out File_Backend;
                                 Start_Position : Positive)
                                 return Positive;

    overriding
    procedure Next_Line (Stream : in out File_Backend);

    overriding
    procedure Next_Char (Stream : in out File_Backend);

    overriding
    procedure Close (Stream : in out File_Backend);

    No_Line_Found : exception;

private
    package Char_IO is new Ada.Direct_IO (
        Element_Type => Character);

    type File_Backend is new Backend_Stream with
    record
        Filename : Unbounded_String;
        File : Char_IO.File_Type;
        Current_Character : Character;
    end record;

end Apagerlib.File_Backend;
