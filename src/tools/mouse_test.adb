--  mouse_test.adb ---

--  Copyright 2022 cnngimenez
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
with Console;
with Mouse;
use Mouse;

procedure Mouse_Test is
    procedure Put_Code (Code : Code_Type);
    procedure Put_Characters (Codes : String);

    procedure Put_Characters (Codes : String) is
    begin
        Put ("^[" & Codes (Codes'First + 1 .. Codes'Last));
        Put (" ");
        for C of Codes loop
            Put (Character'Pos (C)'Image & " ");
        end loop;
    end Put_Characters;

    procedure Put_Code (Code : Code_Type) is
    begin
        if Code.Invalid then
            Put_Line ("Invalid code.                          ");
        else
            Put_Line ("( B: " & Code.B'Image
                        & ", X: " & Code.X'Image
                        & ", Y: " & Code.Y'Image
                        & ", M: " & Code.M & "("
                        & Character'Pos (Code.M)'Image
                        & "))                                  ");
        end if;
    end Put_Code;

    Codes : String (1 .. 15);
    I : Positive := Codes'First;
    C : Character;
    Num : Natural := 0;
begin
    Enable_Mouse;
    Console.Cursor_Position (0, 0);

    loop
        Get_Immediate (C);
        --  Put (C & " ");

        Codes (I) := C;
        I := I + 1;

        if C = 'm' or else C = 'M' or else I > Codes'Last then
            Put_Characters (Codes);
            Put_Code (String_To_Code (Codes));
            I := Codes'First;

            Num := Num + 1;
            if Num >= 10 then
                Console.Cursor_Position (0, 0);
                Num := 0;
            end if;

            Codes := Codes'Length * ' ';
        end if;

        exit when C = 'q';
    end loop;

    Disable_Mouse;
end Mouse_Test;
