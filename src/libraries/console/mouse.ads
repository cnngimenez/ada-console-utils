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
--  along with this program.  If not, see <http://www.gnu.org/Licenses/>.

-------------------------------------------------------------------------

--  Subprograms to process Mouse Events.
--
package Mouse is
    type Mouse_Event_Type is record
        Button_1_Pressed : Boolean;
        Button_2_Pressed : Boolean;
        Button_3_Pressed : Boolean;
        Release : Boolean;
        Shift_Pressed : Boolean;
        Control_Pressed : Boolean;
        Meta_Pressed : Boolean;

        X : Natural;
        Y : Natural;

        Invalid : Boolean;
    end record;

    type Byte is mod 256;

    type Code_Type is record
        B : Byte;
        X : Natural;
        Y : Natural;
        M : Character;
        Invalid : Boolean;
    end record;

    function String_To_Code (Str : String) return Code_Type;
    --  function Code_To_String (Code : Code_Type) return String;

    function Codes_To_Event (Code : Code_Type) return Mouse_Event_Type;
    --  function Event_To_Codes (Mouse_Event : Mouse_Event_Type) return String;

    function String_To_Event (Str : String) return Mouse_Event_Type
        is (Codes_To_Event (String_To_Code (Str)));

    procedure Enable_Mouse;
    --  Tell the terminal to enable Mouse events.
    --  It can be captured with Ada.Text_IO.Get_Immediate repeatedly.

    procedure Disable_Mouse;
    --  Tell the terminal to disable Mouse Events.
    --
    --  Disable the Mouse movement Report.

    Invalid_Mouse_Event : constant Mouse_Event_Type :=
        (Button_1_Pressed => False,
         Button_2_Pressed => False,
         Button_3_Pressed => False,
         Release => False,
         Shift_Pressed => False,
         Control_Pressed => False,
         Meta_Pressed => False,
         X => 0,
         Y => 0,
         Invalid => True);

private

    type String_Array_Index is range 1 .. 3;
    type String_Array is array (String_Array_Index) of String (1 .. 6);

    function Split (Codes : String) return String_Array;
    --  Split the string "^[[<bb;xx;yym into strings ("bb", "xx", "yym").

end Mouse;
