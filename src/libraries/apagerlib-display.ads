--  apagerlib-display.ads ---

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

with Apagerlib.Pages;
use Apagerlib.Pages;

package Apagerlib.Display is

    type Display_Options is
    record
        Columns : Positive;
        Lines : Positive;
        Truncate : Boolean;
        --  Truncate long Lines?
        --  True: truncate the long lines.
        --  False: show the rest of the line below.
        Only_Visible : Boolean;
        --  Print only visible Characters.
        --  Or, ignore non-visible characters when printing.
    end record;

    Default_Display_Options : constant Display_Options := (
      Columns => 60,
      Lines => 60,
      Truncate => False,
      Only_Visible => True);

    procedure Print_Screen
        (Memory : in out Page_Memory;
         Top_Byte : Positive := 1;
         Options : Display_Options := Default_Display_Options);

    procedure Show_Page
        (Page : Page_Type;
         Start : Positive := 1;
         Options : Display_Options := Default_Display_Options);

end Apagerlib.Display;
