--  apagerlib-pages.adb ---  -*- mode: alight; -*-

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
with Ada.Characters.Latin_1;
with Ada.Text_IO;
use Ada.Text_IO;

package body Apagerlib.Pages is

    procedure Clear_Page (Page : in out Page_Type) is
    begin
        for I in Page_Index'Range loop
            Page.Data (I) := Character'Val (0);
        end loop;
        Page.Length := 0;
        Page.Line_Start := 1;
        Page.Line_End := 1;
    end Clear_Page;

    procedure Get_Page (Page : out Page_Type; Line_Start : Positive) is
        use Ada.Characters.Latin_1;

        C : Character;
    begin
        Page.Clear_Page;
        Page.Line_Start := Line_Start;
        Page.Line_End := Line_Start;

        if End_Of_File then
            raise No_Page_Loaded;
        end if;

        Page.Length := 0;
        while not End_Of_File and then Page.Length < Page_Limit loop
            Get_Immediate (C);

            Page.Length := Page.Length + 1;
            Page.Data (Page_Index (Page.Length)) := C;

            if C = LF or else C = CR then
                Page.Line_End := Page.Line_End + 1;
            end if;
        end loop;
    end Get_Page;

end Apagerlib.Pages;
