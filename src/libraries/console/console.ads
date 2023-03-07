--  console.ads ---

--  Copyright 2019 cnngimenez
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

--  Ada implementation for VT102 and other terminal Controls.
--
--  See manpage console_codes (4) for more information.  Also Wikipedia
--  article https://en.wikipedia.org/wiki/ANSI_escape_code has More
--  explanation and references.
package Console is
    type Colour_Type is (Black, Red, Green, Yellow, Blue,
                         Magenta, Cyan, White, Normal);

    type Alt_Font_Type is range 1 .. 9;

    type RGB_Number_Type is range 0 .. 255;

    type RGB_Colour_Type is record
        Red : RGB_Number_Type;
        Green : RGB_Number_Type;
        Blue : RGB_Number_Type;
    end record;

    type Colour_8bit_Type is range 0 .. 255;

    --  --------------------------------------------------
    --  CSI Public Sequences - Set Graphic Rendition (SGR)
    --  --------------------------------------------------

    --  Code 0
    procedure Reset_All;
    procedure Bold;
    procedure Faint;
    procedure Italic;
    procedure Underline;
    procedure Blink;
    procedure Rapid_Blink;
    procedure Reverse_Video;
    procedure Conceal;
    procedure Crossed_Out;

    --  Code 10
    procedure Primary_Font;
    procedure Alternative_Font (Font_Number : Alt_Font_Type);

    --  Code 20
    procedure Fraktur;
    procedure Doubly_Underline; --  Also Bold_Off.
    procedure Normal_Colour; --  Also normal intensity
    procedure Not_Italic; --  Also Not_Fraktur;
    procedure Underline_Off;
    procedure Blink_Off;
    procedure Reverse_Video_Off; --  Inverse off.
    procedure Reveal;
    procedure Crossed_Out_Off; --  Not crossed out

    --  Code 30
    procedure Set_Colour (C : Colour_Type; Bright : Boolean := False);
    procedure Set_RGB_Colour (R, G, B : RGB_Number_Type);
    procedure Set_RGB_Colour (Colour : RGB_Colour_Type);
    procedure Set_8bit_Colour (Num : Colour_8bit_Type);
    procedure Default_Colour;  --  Also Colour_Off;

    --  Code 40
    procedure Set_Background (C : Colour_Type; Bright : Boolean := False);
    procedure Set_RGB_Background (R, G, B : RGB_Number_Type);
    procedure Set_RGB_Background (Colour : RGB_Colour_Type);
    procedure Set_8bit_Background (Num : Colour_8bit_Type);
    procedure Default_Background; --  Also Background_Off

    --  Code 50
    procedure Framed;
    procedure Encircled;
    procedure Overlined;
    procedure Not_Framed; --  Also Not_Encircled.
    procedure Not_Overlined;

    --  Code 60
    procedure Ideogram_Underline;
    procedure Ideogram_Double_Underline;
    procedure Ideogram_Overline;
    procedure Ideogram_Double_Overline;
    procedure Ideogram_Stress;
    procedure Ideogram_Off;

    --  Code 90
    procedure Set_Bright_Colour (Colour : Colour_Type);
    procedure Set_Bright_Background (Colour : Colour_Type);

    --  Aliases

    procedure Bold_Off renames Doubly_Underline;
    procedure Normal_Intensity renames Normal_Colour;
    procedure Not_Fraktur renames Not_Italic;
    procedure Inverse_Off renames Reverse_Video_Off;
    procedure Not_Crossed_Out renames Crossed_Out_Off;
    procedure Colour_Off renames Default_Colour;
    procedure Background_Off renames Default_Background;
    procedure Not_Encircled renames Not_Framed;

    type Code_Type is range 0 .. 255;

    procedure Put_SGR (Code : Code_Type;
                       Parameter : String := "");

    procedure Cursor_Up (Steps : Natural := 1);
    procedure Cursor_Down (Steps : Natural := 1);
    procedure Cursor_Forward (Steps : Natural := 1);
    procedure Cursor_Back (Steps : Natural := 1);

    procedure Cursor_Next_Line (Steps : Natural := 1);
    procedure Cursor_Previous_Line (Steps : Natural := 1);

    procedure Cursor_Horizontal (Steps : Natural := 1);

    procedure Cursor_Position (Row, Column : Natural := 1);

    type Erase_Display_Type is (From_Cursor_To_End,
                                From_Cursor_To_Beginning,
                                Entire_Screen,
                                Entire_Screen_And_Scrollback);
    procedure Erase_Display (What : Erase_Display_Type);

    type Erase_Line_Type is (From_Cursor_To_End,
                             From_Cursor_To_Beginning,
                             Entire_Line);
    procedure Erase_Line (What : Erase_Line_Type);

    procedure Scroll_Up (Steps : Natural := 1);
    procedure Scroll_Down (Steps : Natural := 1);

    procedure Aux_Port_On;
    procedure Aux_Port_Off;

    procedure Device_Status_Report;
    procedure Save_Cursor_Position;
    procedure Restore_Cursor_Position;

    function Bg_Colour_To_Ansi_Bright (C : Colour_Type) return String;
    function Bg_Colour_To_Ansi (C : Colour_Type) return String;
    function Colour_To_Ansi (C : Colour_Type) return String;
    function Colour_To_Ansi_Bright (C : Colour_Type) return String;

    --  --------------------------------------------------
    --  DEC Private Sequences
    --  --------------------------------------------------

    --  CSI numbers in this case are "ESC[?" characters.

    --  Codes 0:

    procedure Cursor_Keys_Sends_Esc_O (Enable : Boolean := False);
    --  Send ESC O instead ESC [ when cursor key is pressed (CSI 1).
    --
    --  DECCKM.

    type Column_Mode_Type is (Column_80, Column_132);

    procedure Column_Mode_Switch (
        Column_Mode : Column_Mode_Type := Column_80);
    --  80/132 column mode switch (CSI 3).
    --
    --  DECCOLM.

    procedure Reverse_Video (Enable : Boolean := False);
    --  Enable/disable reverse video (CSI 5).
    --
    --  DECSCNM.

    procedure Cursor_Address_Scrollig_Region (Enable : Boolean := False);
    --  Cursor address relative to upper left corner (CSI 6).
    --
    --  DECOM.

    procedure Autowrap (Enable : Boolean := True);
    --  Enable/disable autowrap (CSI 7).
    --
    --  DECAWM.  Characters printed after column 80 would be printed at
    --  the firts column at the next line.

    procedure Autorepeat_Keyboard (Autorepeat : Boolean := True);
    --  Enable/disable keyboard autorepeat (CSI 8)
    --
    --  DECARM.

    --  Codes 20:

    procedure Cursor_Visible (Visible : Boolean := True);
    --  Make the cursor visible (CSI 25).
    --
    --  DECTECM.

    --  CSI 1000: check mouse.ads library!

    --  Aliases

    procedure Hide_Cursor;
    procedure Show_Cursor;

    --  --------------------------------------------------
    --  XTerm-compatible window manipulation
    --  --------------------------------------------------

    --  The following are "CSI Code ; Par1 ; Par2 t" Codes.  They are
    --  mostly used on XTerm, and other terminal emulators compatible
    --  to its specifications.

    Procedure Deiconify_Window;
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

    procedure Report_Window_Position;
    --  Code 13

    procedure Report_Window_Size_In_Pixels;
    --  Code 14

    procedure Report_Window_Text_Area_Size;
    --  Code 18
    --
    --  Send a report request of the window texta rea size in
    --  characters.

    procedure Report_Screen_Size;
    --  Code 19
    --
    --  Send a report request of the screen size in characters.

    procedure Report_Window_Icon_Label;
    --  Code 20

    procedure Report_Window_Title;
    --  Code 21

    procedure Resize_To_Lines (Lines : Positive);
    --  Codes >= 24

end Console;
