--  apagerlib-frontend-minibuffers.ads ---

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

package Apagerlib.Frontend.Minibuffers is

    type Minibuffer_Type is tagged record
        Position_Column, Position_Line : Positive;
        --  Where to start drawing the minibuffer.
        Width, Height : Positive;
        --  Width and height in characters (columns) and lines.
        Meta_X : Boolean;
        --  Is M-x pressed?
        Message : Unbounded_String;
    end record;

    function Minibuffer_String (Minibuffer : Minibuffer_Type)
        return Unbounded_String;

    procedure Put_Minibuffer (Minibuffer : Minibuffer_Type);

    Default_Minibuffer : constant Minibuffer_Type := (
        Position_Column => 1,
        Position_Line => 25,
        Width => 25,
        Height => 25,
        Meta_X => False,
        Message => To_Unbounded_String (""));

end Apagerlib.Frontend.Minibuffers;
