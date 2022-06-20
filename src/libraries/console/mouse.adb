--  mouse.adb ---

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
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package body Mouse is

    procedure Disable_Mouse is
    begin
        Put (ESC & "[?1000l");
    end Disable_Mouse;

    procedure Enable_Mouse is
    begin
        Put (ESC & "[?1003h" & ESC & "[?1015h" & ESC & "[?1006h");
    end Enable_Mouse;

    function Split (Codes : String) return String_Array is
        procedure Read_Num (Index : in out Positive; Result : out String);

        --  Codes: 2;75;27m or 35;75;27M
        Index : Positive := Codes'First;
        Substring : String (1 .. 2);
        Results : String_Array;
        Result_Index : String_Array_Index := String_Array_Index'First;

        procedure Read_Num (Index : in out Positive; Result : out String) is
        begin
            if Codes (Index + 1) = ';' then
                Result (Result'First) := '0';
                Result (Result'First + 1) := Codes (Index);
                Index := Index + 2;
            else
                Result (Result'First) := Codes (Index);
                Result (Result'First + 1) := Codes (Index + 1);
                Index := Index + 3;
            end if;
        end Read_Num;
    begin
        Read_Num (Index, Substring);
        Results (Result_Index) := '0' & Substring;
        Result_Index := Result_Index + 1;

        Read_Num (Index, Substring);
        Results (Result_Index) := '0' & Substring;
        Result_Index := Result_Index + 1;

        Read_Num (Index, Substring);
        Results (Result_Index) := Substring & Codes (Index);
        Result_Index := Result_Index + 1;

        return Results;
    end Split;

    --  ^[ is ESC
    --  ^[[ is CSI (Control squence introducer
    --
    --  ^[[<2;75;27m^[[<0;75;27M^[[<0;75;27m^[[<1;75;27M^[[<1;75;27m
    --  ^[[<35;69;39M

    function String_To_Code (Str : String) return Code_Type is
        Result : Code_Type;
        Third_Str : String (1 .. 3);
        Splitted : String_Array;
    begin
        Result.Invalid := True;
        if Str (Str'First) /= ESC
          or else Str (Str'First + 1) /= '['
          or else Str (Str'First + 2) /= '<'
        then
            return Result;
        end if;

        --  Tail: Remove the ESC & "[<" characters.
        Splitted := Split (Tail (Str, Str'First + 3));

        Result.B := Positive'Value (Splitted (1));

        Result.X := Positive'Value (Splitted (2));

        Third_Str := Splitted (3);
        Result.Y := Positive'Value (Third_Str (1) & Third_Str (2));

        Result.M := Third_Str (3);

        Result.Invalid := False;
        return Result;
    end String_To_Code;

end Mouse;
