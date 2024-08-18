--  apagerlib-frontend-minibuffers.adb ---

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

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Console.CSI_Codes;
use Console.CSI_Codes;

package body Apagerlib.Frontend.Minibuffers is

    function Minibuffer_String (Minibuffer : Minibuffer_Type)
        return Unbounded_String
    is ((if Minibuffer.Meta_X then "M-x: " else "")
        & Minibuffer.Message);

    procedure Put_Minibuffer (Minibuffer : Minibuffer_Type) is
        Str : Unbounded_String;
    begin
        Cursor_Position (Minibuffer.Position_Line, Minibuffer.Position_Column);
        Str := Minibuffer_String (Minibuffer);

        Put (To_String (Str)
            & (Minibuffer.Width - Length (Str)) * ' ');

        --  Restore cursor position.
        if Minibuffer.Meta_X then
            Cursor_Position (Minibuffer.Position_Line,
                             Minibuffer.Position_Column + 5);
        else
            Cursor_Position (Minibuffer.Position_Line,
                             Minibuffer.Position_Column);
        end if;
    end Put_Minibuffer;

    procedure Reset (Minibuffer : in out Minibuffer_Type) is
    begin
        Minibuffer.Meta_X := False;
        Minibuffer.Message := To_Unbounded_String ("");
    end Reset;

    procedure Set_Message (Minibuffer : in out Minibuffer_Type;
                           Message : String) is
    begin
        Minibuffer.Message := To_Unbounded_String (Message);
    end Set_Message;

end Apagerlib.Frontend.Minibuffers;
