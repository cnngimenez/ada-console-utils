--  apagerlib-frontend-modelines.adb ---

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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Console.SGR;
with Console.CSI_Codes;
use Console.CSI_Codes;

package body Apagerlib.Frontend.Modelines is

    function Modeline_String (Modeline : Modeline_Type)
        return Unbounded_String
    is (To_Unbounded_String (Modeline.Top_Byte'Image & " "
            --  & Modeline.Current_Line'Image & " "
            & (if Modeline.Truncate = Truncate then "-T- "
               else "-\- "))
            & Modeline.Filename & " "
            & Modeline.More_Text);

    procedure Put_Modeline (Modeline : Modeline_Type) is
        Data : Unbounded_String;
    begin
        Cursor_Position (Modeline.Line_Position, 1);
        Console.SGR.Reverse_Video;

        Data := Modeline_String (Modeline);
        Put_Line (To_String (Data)
            & (Modeline.Width - Length (Data)) * ' ');

        Console.SGR.Reset_All;
    end Put_Modeline;

end Apagerlib.Frontend.Modelines;
