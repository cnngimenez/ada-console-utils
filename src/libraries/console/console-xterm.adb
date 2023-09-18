--  console-xterm.adb ---

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

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;
with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Unbounded;

package body Console.Xterm is

    procedure Deiconify_Window is
    begin
        Put (ESC & "[1t");
    end Deiconify_Window;

    procedure Iconify_Window is
    begin
        Put (ESC & "[2t");
    end Iconify_Window;

    procedure Lower_Window is
    begin
        Put (ESC & "[6t");
    end Lower_Window;

    procedure Maximize_Window is
    begin
        Put (ESC & "[9;1t");
    end Maximize_Window;

    procedure Move_Window (X, Y : Positive) is
    begin
        Put (ESC & "[3;"
            & Trim (X'Image, Both) & ";"
            & Trim (Y'Image, Both) & "t");
    end Move_Window;

    procedure Parse_Parameters (Str : String;
                                Parameters : out Parameters_Type;
                                Valid : out Boolean)
    is
        use Ada.Strings.Unbounded;

        I : Positive;
        Tempstr : Unbounded_String := To_Unbounded_String ("");
        First_Param : Boolean := False;
    begin
        Valid := True;
        I := Str'First;
        while Valid and then I <= Str'Last loop

            case Str (I) is
            when ';' =>
                --  Temptsr has the first parameter.
                if not First_Param then
                    First_Param := True;

                    Parameters.Parameter_1 :=
                        Positive'Value (To_String (Tempstr));
                    Tempstr := To_Unbounded_String ("");
                else
                    --  This is the second ';' found.  String is invalid.
                    Valid := False;
                end if;
            when '0' .. '9' =>
                Tempstr := Tempstr & Str (I);
            when others =>
                Valid := False;
            end case;

        end loop;

        if Valid then
            Parameters.Parameter_2 :=
                Positive'Value (To_String (Tempstr));
        end if;

    end Parse_Parameters;

    function Parse_Position (Str : String) return Screen_Position_Type
    is
        Screen_Position : Screen_Position_Type;
        Parameters : Parameters_Type;
        Valid : Boolean;
    begin
        Parse_Parameters (Str, Parameters, Valid);

        Screen_Position.Valid := Valid;
        if Valid then
            Screen_Position.X := Parameters.Parameter_1;
            Screen_Position.Y := Parameters.Parameter_2;
        end if;

        return Screen_Position;
    end Parse_Position;

    function Parse_Screen_Size_Codes (Str : String)
        return Screen_Size_Type
    is
        Screen_Size : Screen_Size_Type;
    begin
        if not (Str (Str'First .. Str'First + 3) = CSI & "9;"
                and then Str (Str'Last) = 't')
        then
            Screen_Size.Valid := False;
            return Screen_Size;
        end if;

        return Parse_Size (Str (Str'First + 4 .. Str'Last - 1));
    end Parse_Screen_Size_Codes;

    function Parse_Size (Str : String) return Screen_Size_Type is
        Screen_Size : Screen_Size_Type;
        Parameters : Parameters_Type;
        Valid : Boolean;
    begin
        Parse_Parameters (Str, Parameters, Valid);

        Screen_Size.Valid := Valid;
        if Valid then
            Screen_Size.Height := Parameters.Parameter_1;
            Screen_Size.Width := Parameters.Parameter_2;
        end if;

        return Screen_Size;
    end Parse_Size;

    function Parse_Window_Icon_Label_Codes (Str : String) return String is
    begin
        --  Check the OSC codes and ST codes.
        if not (Str (Str'First .. Str'First + 2) = OSC & "L"
                and then (Str (Str'Last - 1 .. Str'Last) = ST))
        then
            return "";
        end if;

        return Str (Str'First + 3 .. Str'Last - 2);
    end Parse_Window_Icon_Label_Codes;

    function Parse_Window_Position_Codes (Str : String)
        return Screen_Position_Type
    is
        Screen_Position : Screen_Position_Type;
    begin
        --  Valid codes are : "CSI 3 ; X ; Y t"
        if not (Str (Str'First .. Str'First + 3) = CSI & "3;"
                and then Str (Str'Last) = 't')
        then
            Screen_Position.Valid := False;
            return Screen_Position;
        end if;

        return Parse_Position (Str (Str'First + 4 .. Str'Last - 1));
    end Parse_Window_Position_Codes;

    function Parse_Window_Size_In_Pixels_Codes (Str : String)
        return Screen_Size_Type
    is
        Screen_Size : Screen_Size_Type;
    begin
        if not (Str (Str'First .. Str'First + 3) = CSI & "4;"
                and then Str (Str'Last) = 't')
        then
            Screen_Size.Valid := False;
            return Screen_Size;
        end if;

        return Parse_Size (Str (Str'First + 4 .. Str'Last - 1));
    end Parse_Window_Size_In_Pixels_Codes;

    function Parse_Window_State_Codes (Str : String)
        return Window_State_Type
    is
    begin
        if Str = CSI & "1t" then
            return Open;
        elsif Str = CSI & "2t" then
            return Iconified;
        else
            return Invalid;
        end if;
    end Parse_Window_State_Codes;

    function Parse_Window_Text_Area_Size_Codes (Str : String)
        return Screen_Size_Type
    is
        Screen_Size : Screen_Size_Type;
    begin
        if not (Str (Str'First .. Str'First + 3) = CSI & "8;"
                and then Str (Str'Last) = 't')
        then
            Screen_Size.Valid := False;
            return Screen_Size;
        end if;

        return Parse_Size (Str (Str'First + 4 .. Str'Last - 1));
    end Parse_Window_Text_Area_Size_Codes;

    function Parse_Window_Title_Codes (Str : String) return String
    is
    begin
        --  Check the OSC codes and ST codes.
        if not (Str (Str'First .. Str'First + 2) = OSC & "l"
                and then (Str (Str'Last - 1 .. Str'Last) = ST))
        then
            return "";
        end if;

        return Str (Str'First + 3 .. Str'Last - 2);
    end Parse_Window_Title_Codes;

    procedure Raise_Window is
    begin
        Put (ESC & "[5t");
    end Raise_Window;

    procedure Refresh_Window is
    begin
        Put (ESC & "[7t");
    end Refresh_Window;

    procedure Report_Screen_Size is
    begin
        Put (ESC & "[19t");
    end Report_Screen_Size;

    procedure Report_Window_Icon_Label is
    begin
        Put (ESC & "[20t");
    end Report_Window_Icon_Label;

    procedure Report_Window_Position is
    begin
        Put (ESC & "[13t");
    end Report_Window_Position;

    procedure Report_Window_Size_In_Pixels is
    begin
        Put (ESC & "[14t");
    end Report_Window_Size_In_Pixels;

    procedure Report_Window_State is
    begin
        Put (ESC & "[11t");
    end Report_Window_State;

    procedure Report_Window_Text_Area_Size is
    begin
        Put (ESC & "[18t");
    end Report_Window_Text_Area_Size;

    procedure Report_Window_Title is
    begin
        Put (ESC & "[21t");
    end Report_Window_Title;

    procedure Resize_Text_Area (Height, Width : Positive) is
    begin
        Put (ESC & "[8;"
            & Trim (Height'Image, Both) & ";"
            & Trim (Width'Image, Both) & "t");
    end Resize_Text_Area;

    procedure Resize_To_Lines (Lines : Positive) is
    begin
        if Lines < 24 then
            return;
        end if;
        Put (ESC & "["
            & Trim (Lines'Image, Both) & "t");
    end Resize_To_Lines;

    procedure Resize_Window (Height, Width : Positive) is
    begin
        Put (ESC & "[4;"
            & Trim (Height'Image, Both) & ";"
            & Trim (Width'Image, Both) & "t");
    end Resize_Window;

    procedure Restore_Maximized_Window is
    begin
        Put (ESC & "[9;0t");
    end Restore_Maximized_Window;

end Console.Xterm;
