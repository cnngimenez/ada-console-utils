--  apagerlib-pages.adb ---

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

    procedure Get_Page (Page : out Page_Type) is
        I : Integer := 1;
    begin
        while not End_Of_File and then I < Page_Limit loop
            Get_Immediate (Page (I));
            I := I + 1;
        end loop;
    end Get_Page;

end Apagerlib.Pages;
