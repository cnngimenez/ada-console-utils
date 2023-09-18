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
with Ada.Characters.Handling;
use Ada.Characters.Handling;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;

package body Mouse is

    function Codes_To_Event (Code : Code_Type) return Mouse_Event_Type
    is
        Event : Mouse_Event_Type;
    begin
        if Code.Invalid then
            return Invalid_Mouse_Event;
        end if;

        Event.Invalid := False;

        Event.Release := Code.M = 'M';
        Event.X := Code.X;
        Event.Y := Code.Y;

        Event.Button_1_Pressed :=
            (Code.B and Byte (2#0000_0011#)) = 0;
        Event.Button_2_Pressed :=
            (Code.B and Byte (2#0000_0011#)) = 1;
        Event.Button_3_Pressed :=
            (Code.B and Byte (2#0000_0011#)) = 2;

        Event.Shift_Pressed := (Code.B and 2#0001_0000#) /= 0;
        Event.Meta_Pressed := (Code.B and 2#0010_0000#) /= 0;
        Event.Control_Pressed := (Code.B and 2#0100_0000#) /= 0;

        return Event;
    end Codes_To_Event;

    procedure Disable_Mouse is
    begin
        Put (ESC & "[?1000l");
    end Disable_Mouse;

    procedure Enable_Mouse is
    begin
        --  1003 : Report all mouse Tracking.
        --  1015 : Report mouse tracking in

        Put (ESC & "[?1000h" & ESC & "[?1003h"
            & ESC & "[?1015h" & ESC & "[?1006h");
    end Enable_Mouse;

    function Split (Codes : String) return String_Array is
        procedure Parse_Num (Index : in out Positive; Result : out String);
        --  Parse a number string strating from the given Index.
        --
        --  For example, If Codes is "aa20bb", then Parse_Num (3, R)
        --  returns R = "20" and Index = 5.

        procedure Initialise (Result : out String);
        --  Fill Result with a Spaces.

        procedure Parse_M (Index : in out Positive; Result : out String);
        --  Parse the last character of the code.

        --  Codes: 2;75;27m or 35;75;27M
        --  Numbers can be from 0 to 100 or more.
        Index : Positive := Codes'First;
        Substring : String (1 .. 5);
        Results : String_Array;
        Result_Index : String_Array_Index := String_Array_Index'First;

        procedure Initialise (Result : out String) is
        begin
            for I in Result'Range loop
                Result (I) := ' ';
            end loop;
        end Initialise;

        procedure Parse_M (Index : in out Positive; Result : out String)
        is
        begin
            Initialise (Result);

            if Index > Codes'Last
               --  or else Codes (Index) /= 'm'
               --  or else Codes (Index) /= 'M'
            then
                --  Something is not right...
                return;
            end if;

            Result (Result'First) := Codes (Index);
            Index := Index + 1;
        end Parse_M;

        procedure Parse_Num (Index : in out Positive; Result : out String)
        is
            Result_I : Positive := Result'First;
        begin
            Initialise (Result);
            while Index <= Codes'Last and then Result_I <= Result'Last
              and then Is_Digit (Codes (Index))
            loop
                Result (Result_I) := Codes (Index);
                Index := Index + 1;
                Result_I := Result_I + 1;
            end loop;
            --  Put_Line (Index'Image & ": """ & Result & """");
        end Parse_Num;

    begin
        --  Code syntax: ^[[<bbb;xxx;yyym

        --  Put_Line ("Split (""" &  Codes & """)");

        --  Parse "bbb"
        Parse_Num (Index, Substring);
        Results (Result_Index) := Substring;
        Result_Index := Result_Index + 1;
        Index := Index + 1; --  Ignore ';'

        --  Parse "xxx"
        Parse_Num (Index, Substring);
        Results (Result_Index) := Substring;
        Result_Index := Result_Index + 1;
        Index := Index + 1; --  Ignore ';'

        --  Parse "yyy"
        Parse_Num (Index, Substring);
        Results (Result_Index) := Substring;
        Result_Index := Result_Index + 1;

        --  Parse "m"
        Parse_M (Index, Substring);
        Results (Result_Index) := Substring;

        return Results;
    end Split;

    --  ^[ is ESC
    --  ^[[ is CSI (Control squence introducer)
    --
    --  ^[[<2;75;27m^[[<0;75;27M^[[<0;75;27m^[[<1;75;27M^[[<1;75;27m
    --  ^[[<35;69;39M

    function String_To_Code (Str : String) return Code_Type is
        Result : Code_Type;
        Third_Str : String (1 .. 5);
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
        Splitted := Split (Tail (Str, Str'Length - 3));

        Result.B := Byte'Value (Splitted (1));

        Result.X := Positive'Value (Splitted (2));

        Third_Str := Splitted (3);
        Result.Y := Positive'Value (Third_Str (1) & Third_Str (2));

        Result.M := Splitted (4)(1);

        Result.Invalid := False;
        return Result;
    end String_To_Code;

end Mouse;
