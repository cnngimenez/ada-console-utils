--  apagerlib-frontend-display.ads ---

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

with Apagerlib.Backend;
use Apagerlib.Backend;

package Apagerlib.Frontend.Display is

    type Display_Options is
    record
        Starting_Column, Starting_Line : Positive;
        --  Where to start drawing.
        Columns, Lines : Positive;
        --  Width and height in characters and lines.
        Truncate : Boolean;
        --  Truncate long Lines?
        --  True: truncate the long lines.
        --  False: show the rest of the line below.
        Only_Visible : Boolean;
        --  Print only visible Characters.
        --  Or, ignore non-visible characters when printing.
    end record;

    Default_Display_Options : constant Display_Options := (
      Starting_Column => 1,
      Starting_Line => 1,
      Columns => 60,
      Lines => 20,
      Truncate => False,
      Only_Visible => True);

    procedure Print_Screen
        (Memory : in out Backend_Stream'Class;
         Top_Byte : Positive := 1;
         Options : Display_Options := Default_Display_Options);

    --  procedure Show_Page
    --      (Page : Page_Type;
    --       Start : Positive := 1;
    --       Options : Display_Options := Default_Display_Options);

end Apagerlib.Frontend.Display;
