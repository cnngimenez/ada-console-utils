--  console_test.adb ---

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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

--  with Ada.Characters.Latin_1;
--  use Ada.Characters.Latin_1;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Text_IO;
use Ada.Text_IO;

with Console.Geometry;
with Console.TTY;
with Emojis;
with Tests.SGR;

procedure Console_Test is
    procedure Test_Emoji;
    procedure Pause;
    procedure Show_Help;

    No_Pause : Boolean := True;

    procedure Pause is
        C : Character;
    begin
        if No_Pause then
            return;
        end if;

        Put_Line ("Press a key to continue.");
        Get_Immediate (C);
    end Pause;

    procedure Show_Help is
    begin
        Put_Line ("Try all console codes implemented in this library.");
        New_Line;
        Put_Line ("Synopsis:");
        New_Line;
        Put_Line ("    console_test [-p|--pause]");
        New_Line;
        Put_Line ("Options:");
        New_Line;
        Put_Line ("  -p | --pause : Print all tests without "
            & "waiting for user key.");
    end Show_Help;

    procedure Test_Emoji is
        use Emojis;
    begin
        Put_Person;
        Put_Skin (Dark);
        Put_Hair (Red_Hair);
        Put_Line ("");
    end Test_Emoji;

begin
    if Argument_Count > 0 then
        if Argument (1) = "-h" or else Argument (1) = "--help" then
            Show_Help;
            return;
        end if;

        if Argument (1) = "-p" or else Argument (1) = "--pause" then
            No_Pause := False;
        end if;
    end if;

    Tests.SGR.Test_Codes_0_To_10;
    Tests.SGR.Test_Codes_10_To_20;
    Tests.SGR.Test_Codes_20_To_30;
    Pause;  --  To much for a screen
    Tests.SGR.Test_Codes_30_To_40;  --  Colour tests
    Pause;
    Tests.SGR.Test_Codes_40_To_50;
    Pause;
    Tests.SGR.Test_Codes_50_To_60;
    Tests.SGR.Test_Codes_60_To_70;
    Tests.SGR.Test_Codes_90_To_110;

    --  --------------------

    Put_Line ("The following elements are not part of the ANSI standard.");
    Put_Line ("Test ligatures:");
    Put_Line ("-> /= != >= <= <> == === ;; <- -< >- || && //");
    Put_Line ("Testing emojis:");
    Test_Emoji;

    Put_Line ("Geometry:");
    Put_Line ("Columns: "
        & Console.Geometry.Get_Columns'Image);
    Put_Line ("Lines: "
        & Console.Geometry.Get_Lines'Image);

    Put_Line ("TTY device file:");
    Put_Line ("TTY Name stdin (0):"
        & Console.TTY.TTY_Name (Console.TTY.Standard_Input_Fd));
    Put_Line ("TTY Name stdout (1):"
        & Console.TTY.TTY_Name (Console.TTY.Standard_Output_Fd));
    Put_Line ("TTY Name stderr (2):"
        & Console.TTY.TTY_Name (Console.TTY.Standard_Error_Fd));
end Console_Test;
