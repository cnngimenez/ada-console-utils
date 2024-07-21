--  apager-keyboard.adb ---

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
--  along with this program.  If not, see <http://www.gnu.org/Licenses/>.

-------------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;
with Ada.Characters.Handling;
use Ada.Characters.Handling;

with Console.TTY;
use Console.TTY;

package body Apagerlib.Keyboard is

    Keyboard_File : File_Type;

    procedure Close_Keyboard is
    begin
        Close (Keyboard_File);
    end Close_Keyboard;

    function CSI_To_Key (Chars : String) return String is
        (if Chars'Length = 3
            and then Chars (Chars'First .. Chars'First + 1) = ESC & "[" then
                (case Chars (Chars'First + 2) is
                    when 'A' => "<up>",
                    when 'B' => "<down>",
                    when 'C' => "<right>",
                    when 'D' => "<left>",
                    when 'F' => "<end>",
                    when 'H' => "<home>",
                    when others => Chars)
        elsif Chars'Length = 4
              and then Chars (Chars'First .. Chars'First + 1) = ESC & "["
              and then Chars (Chars'First + 3) = '~' then
              (case Chars (Chars'First + 2) is
                  when '5' => "<prior>",
                  when '6' => "<next>",
                  when others => Chars)
        else Chars);

    function Get_Line return Unbounded_String is
    begin
        return To_Unbounded_String (Get_Line (Keyboard_File));
    end Get_Line;

    procedure Open_Keyboard is
    begin
        Open (Keyboard_File, In_File, TTY_Name (Standard_Error_Fd));
    end Open_Keyboard;

    function Key_To_Graphical (Chars : String) return String is
        (case Character'Pos (Chars (Chars'First)) is
            when 24 => "C-x",
            when others => Chars);

    function To_Strkey (Chars : String) return String is
        (case Chars'Length is
            when 1 => Key_To_Graphical (Chars),
            when 2 => "M-" & Chars (Chars'First + 1),
            when 3 => CSI_To_Key (Chars),
            when 4 => CSI_To_Key (Chars),
            when others => Chars);

    function Wait_For_Key return Character is
        C : Character;
    begin
        Get_Immediate (Keyboard_File, C);
        return C;
    end Wait_For_Key;

    function Wait_For_Strkey return Unbounded_String is
        Key : Unbounded_String;
        C : Character;
    begin
        C := Wait_For_Key;
        Append (Key, C);

        if C /= ESC then
            return To_Strkey (Key);
        end if;

        --  It is a Meta or arrow key. "ESC ..."
        C := Wait_For_Key;
        Append (Key, C);

        if C /= '[' then
            return To_Strkey (Key);
        end if;

        --  Arrow key: "ESC [ ..."
        C := Wait_For_Key;
        Append (Key, C);

        while Is_Digit (C) loop
            C := Wait_For_Key;
            Append (Key, C);
        end loop;

        return To_Strkey (Key);
    end Wait_For_Strkey;

end Apagerlib.Keyboard;
