--  apagerlib-pages.ads ---

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

    --  type Byte is mod 2**8;
    type Page_Type is array (1 .. Page_Limit) of Character;

    package Page_Vectors is new Ada.Containers.Vectors
      (Element_Type => Page_Type,
       Index_Type => Positive);
    subtype Page_Vector is Page_Vectors.Vector;

    procedure Get_Page (Page : out Page_Type);
    --  Get from standar input a new page.

end Apagerlib.Pages;
