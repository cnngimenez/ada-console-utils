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

    function Current_BIP (Memory : Page_Memory) return Positive
        is (Memory.Current_BIP);

    function Current_Page (Memory : Page_Memory) return Positive
        is (Memory.Current_Page);

    function Current_Byte (Memory : Page_Memory) return Positive
        is (Memory.Current_BIP * Memory.Current_Page);

    procedure Get_Page (Page : out Page_Type; Line_Start : Positive);
    --  Get from standard input a new page.

    function Data (Page : Page_Type; Index : Page_Index) return Character
        is (Page.Data (Index));

    procedure Clear_Page (Page : in out Page_Type) is
    begin
        for I in Page_Index'Range loop
            Page.Data (I) := Character'Val (0);
        end loop;
        Page.Line_Start := 1;
        Page.Line_End := 1;
    end Clear_Page;

    function Get_Byte (Memory : Page_Memory) return Character is
    begin
        return Memory.Pages
            .Element (Memory.Current_Page)
            .Data (Page_Index (Memory.Current_BIP));
    end Get_Byte;

    function Get_Byte (Memory : in out Page_Memory; Index : Positive)
        return Character is
    begin
        return Memory.Get_Page_With_Byte (Index)
            .Data (Page_Index (Index mod Page_Limit));
    end Get_Byte;

    function Get_Page (Memory : in out Page_Memory;
                       Index : Positive)
                       return Page_Type'Class is
        Page : Page_Type;
        Line_Start : Positive := 1;
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
        use Ada.Characters.Latin_1;

        I : Positive := 1;
        C : Character;
    begin
        Page.Clear_Page;
        Page.Line_Start := Line_Start;
        Page.Line_End := Line_Start;

        while not End_Of_File and then I < Page_Limit loop
            Get_Immediate (C);
            Page.Data (Page_Index (I)) := C;

            if C = LF or else C = CR then
                Page.Line_End := Page.Line_End + 1;
            end if;

            I := I + 1;
        end loop;
    end Get_Page;

    function Get_Page_With_Byte (Memory : in out Page_Memory;
                                 Byte_Num : Positive)
                                 return Page_Type'Class is
    begin
        return Get_Page (Memory, Page_Index_With_Byte (Memory, Byte_Num));
    end Get_Page_With_Byte;

    function Get_Page_With_Line (Memory : in out Page_Memory;
                                 Line_Num : Positive)
                                 return Page_Type'Class is
    begin
        return Get_Page (Memory, Page_Index_With_Line (Memory, Line_Num));
    end Get_Page_With_Line;

    procedure Initialise (Pages : in out Page_Memory) is
        Page : Page_Type;
    begin
        Pages.Last_Loaded_Page := 1;
        Pages.Current_Page := 1;
        Pages.Current_BIP := 1;

        Get_Page (Page, 1);
        Pages.Pages.Append (Page);
    end Initialise;

    function Last_Loaded_Page (Page : Page_Memory) return Positive
        is (Page.Last_Loaded_Page);

    function Line_Start (Page : Page_Type) return Positive
        is (Page.Line_Start);

    function Line_End (Page : Page_Type) return Positive
        is (Page.Line_End);

    procedure Load_Next_Page (Memory : in out Page_Memory) is
        Page : Page_Type;
    begin
        Get_Page (Page, 1);
        Memory.Pages.Append (Page);
        Memory.Last_Loaded_Page := Memory.Last_Loaded_Page + 1;
    end Load_Next_Page;

    function Load_Next_Page (Memory : in out Page_Memory)
                             return Page_Type'Class is
    begin
        Memory.Load_Next_Page;
        return Memory.Pages.Last_Element;
    end Load_Next_Page;

    function Next_Byte (Memory : in out Page_Memory) return Character is
    begin
        Memory.Current_BIP := Memory.Current_BIP + 1;

        if Memory.Current_BIP > Page_Limit then
            --  There is no more bytes in the page, use the next page.
            Memory.Current_BIP := 1;
            Memory.Current_Page := Memory.Current_Page + 1;

            if Memory.Current_Page > Memory.Last_Loaded_Page then
                --  No more pages, load a new one.
                Memory.Load_Next_Page;
            end if;
        end if;

        return Memory.Get_Byte;
    end Next_Byte;

    procedure Next_Line (Memory : in out Page_Memory) is
    begin
        Memory.Set_Byte_Index (Next_Line_Byte (Memory, Memory.Current_Byte));
    end Next_Line;

    function Next_Line_Byte (Memory : in out Page_Memory;
                             Start_Byte : Positive)
                             return Positive is
        use Ada.Characters.Latin_1;

        Page : Page_Type;
        Pindex : Positive;
        C : Character;
        I : Positive := 1;
        Count : Positive := Start_Byte;
    begin
        Pindex := Page_Index_With_Byte (Memory, Start_Byte);
        Page := Page_Type (Memory.Get_Page (Pindex));

        I := Start_Byte mod Page_Limit;
        C := Page.Data (Page_Index (I));
        while not End_Of_File and then C /= LF and then C /= CR
        loop
            I := I + 1;
            Count := Count + 1;

            if I > Page_Limit then
                Pindex := Pindex + 1;
                Page := Page_Type (Memory.Get_Page (Pindex));
                I := 1;
            end if;

            C := Page.Data (Page_Index (I));
        end loop;

        if C = LF or else C = CR then
            return Count;
        else
            --  Not found!
            return 1;
        end if;
    end Next_Line_Byte;

    function Page_Index_With_Byte (Memory : Page_Memory; Byte_Num : Positive)
        return Positive
        is ((Byte_Num / Page_Limit) + 1);

    function Page_Index_With_Line (Memory : Page_Memory; Line_Num : Positive)
        return Positive is
        I : Positive := 1;
    begin
        --  Search the given line number through pages
        while I <= Memory.Last_Loaded_Page
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
            return 1;
        end if;

    end Page_Index_With_Line;

    function Previous_Byte (Memory : in out Page_Memory) return Character is
    begin

        if Memory.Current_BIP > 1 then
            Memory.Current_BIP := Memory.Current_BIP - 1;
        else
            --  No more bytes in page, use the previous one.
            if Memory.Current_Page > 1 then
                Memory.Current_Page := Memory.Current_Page - 1;
                Memory.Current_BIP := Page_Limit;
            end if;
        end if;

        return Memory.Get_Byte;
    end Previous_Byte;

    procedure Previous_Line (Memory : in out Page_Memory) is
    begin
        Memory.Set_Byte_Index
            (Previous_Line_Byte (Memory, Memory.Current_Byte));
    end Previous_Line;

    function Previous_Line_Byte (Memory : in out Page_Memory;
                                 Start_Byte : Positive)
                                 return Positive is
        use Ada.Characters.Latin_1;

        Page : Page_Type;
        Pindex : Positive;
        --  Page index
        C : Character;
        I : Natural := 1;
        --  Index inside the page
        Count : Positive := Start_Byte;
        --  Overall index
    begin
        Pindex := Page_Index_With_Byte (Memory, Start_Byte);
        Page := Page_Type (Memory.Get_Page (Pindex));

        I := Start_Byte mod Page_Limit;
        C := Page.Data (Page_Index (I));
        while not (Count = 1) and then C /= LF and then C /= CR
        loop
            I := I - 1;
            Count := Count - 1;

            if I = 0 then
                Pindex := Pindex - 1;
                Page := Page_Type (Memory.Get_Page (Pindex));
                I := 1;
            end if;

            C := Page.Data (Page_Index (I));
        end loop;

        if C = LF or else C = CR then
            return Count;
        else
            --  Not found!
            return 1;
        end if;
    end Previous_Line_Byte;

    procedure Set_Byte_Index (Memory : in out Page_Memory; Index : Positive) is
        New_Page_Index : constant Positive := Index / Page_Limit + 1;
        New_BIP : constant Positive := Index mod Page_Limit;
    begin
        --  Load pages if needed
        while New_Page_Index > Memory.Last_Loaded_Page loop
            Load_Next_Page (Memory);
        end loop;

        Memory.Current_BIP := New_BIP;
        Memory.Current_Page := New_Page_Index;
    end Set_Byte_Index;
end Apagerlib.Pages;
