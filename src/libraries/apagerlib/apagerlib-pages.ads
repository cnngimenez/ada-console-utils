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

    procedure Get_Page (Page : out Page_Type; Line_Start : Positive);
    --  Get from standard input a new Page.

private
    type Page_Array is array (Page_Index) of Character;

    type Page_Type is tagged
    record
        Line_Start, Line_End : Positive;
        Data :  Page_Array;
    end record;

    procedure Clear_Page (Page : in out Page_Type);

    function Data (Page : Page_Type; Index : Page_Index) return Character
        is (Page.Data (Index));

    function Line_End (Page : Page_Type) return Positive
        is (Page.Line_End);

    function Line_Start (Page : Page_Type) return Positive
        is (Page.Line_Start);

end Apagerlib.Pages;
