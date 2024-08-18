--  apagerlib-display.adb ---

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

with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
use Ada.Characters.Latin_1;
with Ada.Text_IO;
use Ada.Text_IO;

with Console.CSI_Codes;
with Apagerlib.Memories;

package body Apagerlib.Display is

    procedure Put_Char (C : Character);
    procedure Show_No_Truncate (Memory : in out Backend_Stream'Class;
                                Start : Positive := 1;
                                Options : Display_Options);
    procedure Show_Truncate (Memory : in out Backend_Stream'Class;
                             Start : Positive := 1;
                             Options : Display_Options);
    --  procedure Show_No_Truncate (Page : Page_Type;
    --                              Start : Positive := 1;
    --                              Options : Display_Options);
    --  procedure Show_Truncate (Page : Page_Type;
    --                           Start : Positive := 1;
    --                           Options : Display_Options);

    procedure Print_Screen
        (Memory : in out Backend_Stream'Class;
         Top_Byte : Positive := 1;
         Options : Display_Options := Default_Display_Options) is
    begin
        Console.CSI_Codes.Cursor_Position (Options.Starting_Column,
                                           Options.Starting_Line);
        if Options.Truncate then
            Show_Truncate (Memory, Top_Byte, Options);
        else
            Show_No_Truncate (Memory, Top_Byte, Options);
        end if;
    end Print_Screen;

    procedure Put_Char (C : Character) is
    begin
        if C = LF or else C = CR then
            New_Line;
        elsif Ada.Characters.Handling.Is_Graphic (C) then
            Put (C);
        else
            Put (' ');
        end if;
    end Put_Char;

    procedure Show_No_Truncate (Memory : in out Backend_Stream'Class;
                                Start : Positive := 1;
                                Options : Display_Options) is
        Column, Line : Positive := 1;
        C : Character;
    begin
        Memory.Set_Position (Start);

        begin
            C := Memory.Get_Char;
        exception
        when Apagerlib.Memories.No_Byte_Found
             | Apagerlib.Backend.No_More_Char =>
            C := LF;
        end;

        if C /= LF and then C /= CR then
            Put_Char (C);
            Column := Column + 1;
        end if;

        while Line <= Options.Lines
        loop
            if Column = Options.Columns then
                Column := 1;
                Line := Line + 1;
                New_Line;
            end if;

            begin
                C := Memory.Next_Char;
            exception
            when Apagerlib.Memories.No_Byte_Found
                 | Apagerlib.Backend.No_More_Char =>
                C := LF;
            end;

            if C = LF or else C = CR then
                Column := 1;
                Line := Line + 1;
            end if;

            Put_Char (C);

            Column := Column + 1;
        end loop;

    end Show_No_Truncate;

    --  procedure Show_No_Truncate (Page : Page_Type;
    --                              Start : Positive := 1;
    --                              Options : Display_Options) is
    --      Char_Limit : constant Positive := Options.Columns * Options.Lines;
    --      I : Positive := Start;
    --      Column, Line : Positive := 1;
    --      C : Character;
    --  begin
    --      while Line <= Options.Lines and then Column <= Options.Columns
    --            and then I <= Char_Limit and then I <= Page_Limit
    --      loop
    --          if Column = Options.Columns then
    --              Column := 1;
    --              Line := Line + 1;
    --          end if;

    --          C := Page.Data (Page_Index (I));

    --          if C = LF or else C = CR then
    --              Column := 1;
    --              Line := Line + 1;
    --              New_Line;
    --          end if;

    --          if Ada.Characters.Handling.Is_Graphic (C) then
    --              Put (C);
    --          else
    --              Put (' ');
    --          end if;
    --          I := I + 1;
    --          Column := Column + 1;
    --      end loop;
    --  end Show_No_Truncate;

    --  procedure Show_Page (Page : Page_Type;
    --                       Start : Positive := 1;
    --                       Options : Display_Options :=
    --                           Default_Display_Options)
    --  is
    --  begin
    --      if Options.Truncate then
    --          Show_Truncate (Page, Start, Options);
    --      else
    --          Show_No_Truncate (Page, Start, Options);
    --      end if;
    --  end Show_Page;

    --  procedure Show_Truncate (Page : Page_Type;
    --                           Start : Positive := 1;
    --                           Options : Display_Options) is
    --  begin
    --      --  To be implemented.
    --      return;
    --  end Show_Truncate;

    procedure Show_Truncate (Memory : in out Backend_Stream'Class;
                             Start : Positive := 1;
                             Options : Display_Options) is
        Column, Line : Positive := 1;
        C : Character;
    begin
        Memory.Set_Position (Start);

        begin
            C := Memory.Get_Char;
        exception
        when Apagerlib.Memories.No_Byte_Found
             | Apagerlib.Backend.No_More_Char =>
            C := LF;
        end;

        if C /= LF and then C /= CR then
            Put_Char (C);
            Column := Column + 1;
        end if;

        while Line <= Options.Lines loop
            if Column = Options.Columns then
                Column := 1;
                Line := Line + 1;
                Memory.Next_Line;
                New_Line;
            end if;

            begin
                C := Memory.Next_Char;
            exception
            when Apagerlib.Memories.No_Byte_Found
                 | Apagerlib.Backend.No_More_Char =>
                C := LF;
            end;

            if C = LF or else C = CR then
                Column := 1;
                Line := Line + 1;
            end if;

            Put_Char (C);
            Column := Column + 1;
        end loop;
    end Show_Truncate;

end Apagerlib.Display;
