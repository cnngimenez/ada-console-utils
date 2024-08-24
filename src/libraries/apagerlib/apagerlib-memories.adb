--  apagerlib-memories.adb ---

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

with Ada.Exceptions;
use Ada.Exceptions;
with Ada.Text_IO;
use Ada.Text_IO;

package body Apagerlib.Memories is

    overriding
    procedure Close (Memory : in out Page_Memory)
        is null;

    function Current_BIP (Memory : Page_Memory) return Positive
        is (Memory.Current_BIP);

    function Current_Page (Memory : Page_Memory) return Positive
        is (Memory.Current_Page);

    overriding
    function Current_Position (Memory : Page_Memory) return Positive
        is (Memory.Current_BIP + ((Memory.Current_Page - 1) * Page_Limit));

    overriding
    procedure Beginning_Position (Memory : in out Page_Memory) is
    begin
        Memory.Current_BIP := 1;
        Memory.Current_Page := 1;
    end Beginning_Position;

    overriding
    function End_Of_File (Memory : Page_Memory) return Boolean
        is (Ada.Text_IO.End_Of_File
        and then Memory.Current_Page >= Memory.Last_Loaded_Page
        and then Memory.Current_BIP > Memory.Pages.Last_Element.Length);

    overriding
    procedure End_Position (Memory : in out Page_Memory) is
    begin
        while not Ada.Text_IO.End_Of_File loop
            Memory.Load_Next_Page;
        end loop;

        Memory.Current_Page := Positive (Memory.Pages.Length);
        Memory.Current_BIP := Memory.Pages.Last_Element.Length;

        exception
        when No_Next_Page =>
            Memory.Current_Page := Positive (Memory.Pages.Length);
            Memory.Current_BIP := Memory.Pages.Last_Element.Length;
            return;
    end End_Position;

    overriding
    function Get_Char (Memory : in out Page_Memory) return Character is
    begin
        return Memory.Pages
            .Element (Memory.Current_Page)
            .Data (Page_Index (Memory.Current_BIP));
        exception
        when Constraint_Error =>
            Raise_Exception (No_More_Char'Identity,
                "No character on page "
                & Memory.Current_Page'Image
                & " and index "
                & Memory.Current_BIP'Image);
    end Get_Char;

    function Get_Char (Memory : in out Page_Memory; Index : Positive)
        return Character is
    begin
        if Memory.End_Of_File then
            raise No_More_Char;
        end if;
        return Memory.Get_Page_With_Byte (Index)
            .Data (Page_Index (Index mod Page_Limit));
    end Get_Char;

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

            Apagerlib.Pages.Get_Page (Page, Line_Start);
            Memory.Pages.Append (Page);
        end loop;

        Memory.Last_Loaded_Page := Index;
        return Memory.Pages.Element (Index);
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

    function Last_Loaded_Page (Page : Page_Memory) return Positive
        is (Page.Last_Loaded_Page);

    procedure Load_Next_Page (Memory : in out Page_Memory) is
        Page : Page_Type;
    begin
        if Ada.Text_IO.End_Of_File then
            Raise_Exception (No_Next_Page'Identity,
                "Cannot load next page on End_Of_File");
        end if;

        Apagerlib.Pages.Get_Page (Page, 1);
        Memory.Pages.Append (Page);
        Memory.Last_Loaded_Page := Positive (Memory.Pages.Length);

        exception
            when Exc : Apagerlib.Pages.No_Page_Loaded =>
                Raise_Exception (No_Next_Page'Identity,
                    "Next page cannot be loaded. "
                    & Exception_Message (Exc));
    end Load_Next_Page;

    function Load_Next_Page (Memory : in out Page_Memory)
                             return Page_Type'Class is
    begin
        Memory.Load_Next_Page;
        return Memory.Pages.Last_Element;
    end Load_Next_Page;

    overriding
    procedure Next_Char (Memory : in out Page_Memory) is
        Last_BIP : Positive;
    begin
        if Memory.End_Of_File then
            Raise_Exception (No_More_Char'Identity,
                "Cannot Next_Char at End_Of_File (page: "
                & Memory.Current_Page'Image
                & " index: "
                & Memory.Current_BIP'Image
                & ")");
        end if;

        Last_BIP := Memory.Current_BIP;
        Memory.Current_BIP := Memory.Current_BIP + 1;

        if not Memory.End_Of_File
            and then Memory.Current_BIP >
            Memory.Pages.Element (Memory.Current_Page).Length
        then
            --  There is no more bytes in the page, use the next page.
            Memory.Current_BIP := 1;
            Memory.Current_Page := Memory.Current_Page + 1;

            if Memory.Current_Page > Memory.Last_Loaded_Page
            then
                --  No more pages, load a new one.
                Memory.Load_Next_Page;
            end if;
        end if;

        exception
            when Exc : No_Next_Page =>
                Memory.Current_BIP := Last_BIP;
                Memory.Current_Page := Positive (Memory.Pages.Length);
                Memory.Last_Loaded_Page := Positive (Memory.Pages.Length);
                Raise_Exception (No_More_Char'Identity,
                    "Cannot load next page (page: "
                    & Memory.Current_Page'Image
                    & " index: "
                    & Memory.Current_BIP'Image & "). "
                    & Exception_Message (Exc));

    end Next_Char;

    overriding
    procedure Open (Memory : in out Page_Memory) is
        Page : Page_Type;
    begin
        Memory.Last_Loaded_Page := 1;
        Memory.Current_Page := 1;
        Memory.Current_BIP := 1;

        Get_Page (Page, 1);
        Memory.Pages.Append (Page);
    end Open;

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
            raise No_Page_Found;
        end if;

    end Page_Index_With_Line;

    overriding
    procedure Previous_Char (Memory : in out Page_Memory) is
    begin
        if Memory.Current_BIP > 1 then
            Memory.Current_BIP := Memory.Current_BIP - 1;
        else
            --  No more bytes in page, use the previous one.
            if Memory.Current_Page > 1 then
                Memory.Current_Page := Memory.Current_Page - 1;
                Memory.Current_BIP := Page_Limit;
            else
                --  No previous page!
                Raise_Exception (No_More_Char'Identity,
                    "Could not find previous character at first position.");
            end if;
        end if;
    end Previous_Char;

    overriding
    procedure Set_Position (Memory : in out Page_Memory; Position : Positive)
    is
        New_Page_Position : constant Positive := Position / Page_Limit + 1;
        New_BIP : constant Positive := Position mod Page_Limit;
    begin
        --  Load pages if needed
        while New_Page_Position > Memory.Last_Loaded_Page loop
            Load_Next_Page (Memory);
        end loop;

        Memory.Current_BIP := New_BIP;
        Memory.Current_Page := New_Page_Position;
    end Set_Position;

end Apagerlib.Memories;
