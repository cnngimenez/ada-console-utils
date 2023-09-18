--  console-xterm.ads ---

--  Copyright 2023 cnngimenez
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

--  --------------------------------------------------
--  XTerm-compatible window Manipulation
--  --------------------------------------------------

--
--  The following are "CSI Code ; Par1 ; Par2 t" Codes.  They are
--  mostly used on XTerm, and other terminal emulators compatible
--  to its Specifications.
--
--  All Report_* procedures produces output from XTerm.  This output
--  can be parsed with Parse_*_Codes functions declared immediately
--  below their corresponding report procedures.
--
--  Some Parse_*_Codes functions return a specific type.  If the
--  code sequence parsed is not the expected one, the type has a
--  Valid field which will be set to False.
--
--  More information: https://www.xfree86.org/current/ctlseqs.html
--
package Console.Xterm is

    type Screen_Size_Type is
    record
        Valid : Boolean;
        Height, Width : Positive;
    end record;

    type Screen_Position_Type is
    record
        Valid : Boolean;
        X, Y : Positive;
    end record;

    procedure Deiconify_Window;
    --  Code 1

    procedure Iconify_Window;
    --  Code 2

    procedure Move_Window (X, Y : Positive);
    --  Code 3

    procedure Resize_Window (Height, Width : Positive);
    --  Code 4

    procedure Raise_Window;
    --  Code 5

    procedure Lower_Window;
    --  Code 6

    procedure Refresh_Window;
    --  Code 7

    procedure Resize_Text_Area (Height, Width : Positive);
    --  Code 8

    procedure Restore_Maximized_Window;
    --  Code 9 ; 0

    procedure Maximize_Window;
    --  Code 9 ; 1

    procedure Report_Window_State;
    --  Code 11

    type Window_State_Type is (Open, Iconified, Invalid);

    function Parse_Window_State_Codes (Str : String)
        return Window_State_Type;
    --  Valid codes are: "CSI 1 t" or "CSI 2 t"

    procedure Report_Window_Position;
    --  Code 13

    function Parse_Window_Position_Codes (Str : String)
        return Screen_Position_Type;
    --  Valid codes are : "CSI 3 ; X ; Y t"

    procedure Report_Window_Size_In_Pixels;
    --  Code 14

    function Parse_Window_Size_In_Pixels_Codes (Str : String)
        return Screen_Size_Type;
    --  Valid codes are : "CSI 4 ; HEIGHT ; WIDTH t"

    procedure Report_Window_Text_Area_Size;
    --  Code 18
    --
    --  Send a report request of the window texta rea size in
    --  characters.

    function Parse_Window_Text_Area_Size_Codes (Str : String)
        return Screen_Size_Type;
    --  Valid codes are : "CSI 8 ; HEIGHT ; WIDTH t"
    --
    --  Return Screen_Size.Valid as False if the code are invalid.

    procedure Report_Screen_Size;
    --  Code 19
    --
    --  Send a report request of the screen size in Characters.

    function Parse_Screen_Size_Codes (Str : String) return Screen_Size_Type;
    --  Valid codes are : "CSI 9 ; HEIGHT ; WIDTH t"

    procedure Report_Window_Icon_Label;
    --  Code 20

    function Parse_Window_Icon_Label_Codes (Str : String) return String;
    --  Valid codes are: "OSC L TITLE ST"

    procedure Report_Window_Title;
    --  Code 21

    function Parse_Window_Title_Codes (Str : String) return String;
    --  Valid codes are: "OSC l TITLE ST"

    procedure Resize_To_Lines (Lines : Positive);
    --  Codes >= 24

private
    type Parameters_Type is
    record
        Parameter_1, Parameter_2 : Positive;
    end record;

    procedure Parse_Parameters (Str : String;
                                Parameters : out Parameters_Type;
                                Valid : out Boolean);
    --  Parse the string "Number1;Number2" into numbers.
    --
    --  Also check if "Number1" and "Number2" is a valid number.

    function Parse_Size (Str : String) return Screen_Size_Type;
    function Parse_Position (Str : String) return Screen_Position_Type;

end Console.Xterm;
