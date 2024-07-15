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
with Ada.Text_IO;
use Ada.Text_IO;

package body Apagerlib.Pages is

    function Data (Page : Page_Type; Index : Page_Index) return Positive
        is Page.Data (Index);

    function Get_Page (Memory : in out Page_Memory; Index : Positive)
        return Page_Type is

        Page : Page_Type;
        Line_Start : Positive := 0;
    begin
        if Index <= Memory.Last_Loaded_Page then
            --  Page is already loaded, just return it.
            return Memory.Pages.Element (Index);
        end if;

        --  Load pages up to index if they were not loaded.
        for I in Memory.Last_Loaded_Page .. Index loop
            if I - 1 > 0 then
                --  it is not the first page: calculate the new Line_Start
                --  if it is the first page, the Line_Start is 0.
                Line_Start := Memory.Pages.Element (I - 1).Line_End + 1;
            end if;

            Get_Page (Page, Line_Start);
            Memory.Pages.Append (Page);
        end loop;

        Memory.Last_Loaded_Page := Index;
        return Memory.Pages.Element (Index);
    end Get_Page;

    procedure Get_Page (Page : out Page_Type; Line_Start : Positive) is
        I : Integer := 1;
        C : Character;
    begin
        Page.Line_Start := Line_Start;
        Page.Line_End := Line_Start;

        while not End_Of_File and then I < Page_Limit loop
            Get_Immediate (C);
            Page.Data (I) := C;

            if C = LR or else C = CR then
                Page.Line_End := Page.Line_End + 1;
            end if;

            I := I + 1;
        end loop;
    end Get_Page;

    function Get_Page_With_Line (Memory : in out Page_Memory;
                                 Line_Num : Positive)
                                 return Page_Type is
    begin
        return Get_Page (Memory, Page_Index_With_Line (Memory, Line_Num));
    end Get_Page_With_Line;

    function Get_Page_With_Byte (Memory : in out Page_Memory;
                                 Byte_Num : Positive)
                                 return Page_Type is
    begin
        return Get_Page (Memory, Page_Index_With_Byte (Memory, Byte_Num));
    end Get_Page_With_Byte;

    function Line_Start (Page : Page_Type) return Positive
        is Page.Line_Start;

    function Line_End (Page : Page_Type) return Positive
        is Page.Line_End;

    function Page_Index_With_Line (Memory : Page_Memory; Line_Num : Positive)
        return Positive is
        I : Positive := 0;
    begin
        --  Search the given line number through pages
        while I <= Memory.Last_Loaded_Page loop
            and then Line_Num > Memory.Pages (I).Line_End
        loop
            I := I + 1;
        end loop;

        if I <= Memory.Last_Loaded_Page
            and then Memory.Pages (I).Line_Start >= Line_Num
            and then Line_Num <= Memory.Pages (I).Line_End
        then
            --  I is the page index, which page has the queried line
            return I;
        else
            --  Not found!
            return 0;
        end if;

    end Page_Index_With_Line;

    function Page_Index_With_Byte (Memory : Page_Memory; Byte_Num : Positive)
        return Positive is
        Index : Positive;
    begin
        Index :=  (Byte_Num / Page_Limit) + 1;
        return (if Index <= Memory.Last_Loaded_Page then Index else 0);
    end Page_Index_With_Byte;

end Apagerlib.Pages;
