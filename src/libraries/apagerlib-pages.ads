--  apagerlib-pages.ads ---  -*- mode: alight; -*-

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

with Ada.Containers.Vectors;

package Apagerlib.Pages is

    Page_Limit : constant Natural := 6800;

    type Page_Index is range 1 .. Page_Limit;

    --  type Byte is mod 2**8;
    type Page_Type is tagged private;

    function Line_Start (Page : Page_Type) return Positive;
    --  What line number starts in this page?

    function Line_End (Page : Page_Type) return Positive;
    --  What line number ends in this page?

    function Data (Page : Page_Type; Index : Page_Index) return Character;

    --
    --  Page Memory
    --

    type Page_Memory is tagged private;
    --  A Page Memory.
    --
    --  It is a lazy-loaded memory divided by pages. It gets the data from
    --  standard input. See the documentation manual at docs/console.org for
    --  more Information.

    procedure Initialise (Pages : in out Page_Memory);

    function Last_Loaded_Page (Page : Page_Memory) return Positive;

    function Get_Page (Memory : in out Page_Memory;
                       Index : Positive)
                       return Page_Type'Class;
    --  Get the memory page.
    --
    --  Load the pages if it is needed.

    function Page_Index_With_Line (Memory : Page_Memory; Line_Num : Positive)
        return Positive;
    --  Return the page index where the given line number is.

    function Get_Page_With_Line (Memory : in out Page_Memory;
                                 Line_Num : Positive)
                                 return Page_Type'Class;
    --  Search for the page with the given line number position.
    --
    --  Load the pages if it is necessary.

    function Page_Index_With_Byte (Memory : Page_Memory; Byte_Num : Positive)
        return Positive;
    --  Return the page index where the given byte num is.

    function Get_Page_With_Byte (Memory : in out Page_Memory;
                                 Byte_Num : Positive)
                                 return Page_Type'Class;
    --  Search for the page with the given byte number position.
    --
    --  Load the pages if it is necessary.

    function Next_Line_Byte (Memory : in out Page_Memory;
                             Start_Byte : Positive)
                             return Positive;
    --  Search the next line position starting from the given Byte.
    --
    --  Load the pages if it needed.
    function Previous_Line_Byte (Memory : in out Page_Memory;
                                 Start_Byte : Positive)
                                 return Positive;

private
    type Page_Array is array (Page_Index) of Character;

    type Page_Type is tagged
    record
        Line_Start, Line_End : Positive;
        Data :  Page_Array;
    end record;

    package Page_Vectors is new Ada.Containers.Vectors
      (Element_Type => Page_Type,
       Index_Type => Positive);
    subtype Page_Vector is Page_Vectors.Vector;

    type Page_Memory is tagged
    record
        Last_Loaded_Page : Positive := 1;
        Pages : Page_Vector;
    end record;

    procedure Load_Next_Page (Memory : in out Page_Memory);
    function Load_Next_Page (Memory : in out Page_Memory)
                             return Page_Type'Class;
    --  Load the next page from standard in.

    --  function Count_Lines (Page : Page_Type) return Positive;
    --  Count the line number in the page data.
    --
    --  Used to set the Page.Line_End attribute.

end Apagerlib.Pages;
