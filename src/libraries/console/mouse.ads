--  mouse.ads ---

--  Copyright 2022 cnngimenez
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

package Mouse is
    type Mouse_Event_Type is record
        Button_1_Pressed : Boolean;
        Button_2_Pressed : Boolean;
        Button_3_Pressed : Boolean;
        Release : Boolean;
        Shift_Pressed : Boolean;
        Control_Pressed : Boolean;
        Meta_Pressed : Boolean;

        X : Positive;
        Y : Positive;
    end record;

    type Code_Type is record
        B : Positive;
        X : Positive;
        Y : Positive;
        M : Character;
        Invalid : Boolean;
    end record;

    function String_To_Code (Str : String) return Code_Type;
    --  function Codes_To_Event (Code : Code_Type) return Mouse_Event_Type;
    --  function Event_To_Codes (Mouse_Event : Mouse_Event_Type) return String;

    procedure Enable_Mouse;
    procedure Disable_Mouse;

private

    type String_Array_Index is range 1 .. 3;
    type String_Array is array (String_Array_Index) of String (1 .. 3);

    function Split (Codes : String) return String_Array;
    --  Split the string "^[[<bb;xx;yym into strings ("bb", "xx", "yym").

end Mouse;
