--  apager.adb ---

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
with GNAT.Ctrl_C;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Command_Line;

with Console.CSI_Codes;
use Console.CSI_Codes;
with Console.CSI_Private;
use Console.CSI_Private;
with Console.Geometry;
with Apagerlib.Keyboard;
with Apagerlib.Backend;
with Apagerlib.Memories;
with Apagerlib.File_Backend;
with Apagerlib.Frontend.Display;
with Apagerlib.Commands;
with Apagerlib.Frontend.Modelines;
use Apagerlib.Frontend.Modelines;
with Apagerlib.Frontend.Minibuffers;
use Apagerlib.Frontend.Minibuffers;

procedure Apager is

    function To_U (Item : String) return Unbounded_String
        renames To_Unbounded_String;

    procedure Assign_Console_Size;
    procedure Assign_Input;
    procedure Read_Keyboard_Command;
    procedure Quit_Handler;
    procedure Show_Help;
    procedure Update_Modeline;
    procedure Update_Minibuffer;
    procedure Run_Epilogue;
    procedure Do_Next_Line;
    procedure Do_Previous_Line;
    procedure Do_Page_Up;
    procedure Do_Page_Down;

    Buffer : access Apagerlib.Backend.Backend_Stream'Class;
    File_Buffer : aliased Apagerlib.File_Backend.File_Backend;
    Paged_Buffer : aliased Apagerlib.Memories.Page_Memory;

    Commands : Apagerlib.Commands.Command_Map;
    Keys, Command : Unbounded_String;
    Exit_Program : Boolean := False;
    Top_Byte : Positive := 1;
    Options : Apagerlib.Frontend.Display.Display_Options :=
        Apagerlib.Frontend.Display.Default_Display_Options;
    Fixed_Size : Boolean := False;
    Modeline : Modeline_Type := Default_Modeline;
    Minibuffer : Minibuffer_Type := Default_Minibuffer;

    End_Program_Exception : exception;

    procedure Assign_Console_Size is
    begin
        if not Fixed_Size then
            Options.Columns := Console.Geometry.Get_Columns;
            Options.Lines := Console.Geometry.Get_Lines - 3;
        end if;

        exception
            when Console.Geometry.No_Geometry_Information =>
                Put_Line ("Cannot retrieve terminal total lines and columns");

                Options.Columns := 80;
                Options.Lines := 80;
    end Assign_Console_Size;

    procedure Assign_Input is
        use Ada.Command_Line;
        use Apagerlib.File_Backend;
    begin
        if Argument_Count > 0 then
            Buffer := File_Buffer'Access;
            File_Buffer.Set_Filename (Argument (1));
        else
            Buffer := Paged_Buffer'Access;
        end if;
    end Assign_Input;

    procedure Do_Next_Line is
    begin
        Top_Byte := Buffer.Next_Line_Position (Top_Byte + 1);

    exception
        when Apagerlib.Memories.No_Byte_Found => null;
        when Apagerlib.Memories.No_Line_Found => null;
    end Do_Next_Line;

    procedure Do_Page_Down is
        Bottom : constant Positive := Options.Lines - 2;
    begin
        for I in 1 .. Bottom loop
            Do_Next_Line;
        end loop;

        exception
        when Apagerlib.Backend.No_Line_Found =>
            return;
    end Do_Page_Down;

    procedure Do_Page_Up is
        I : Positive := 2;
    begin
        while I < Options.Lines and then Top_Byte > 1 loop
            Top_Byte := Buffer.Previous_Line_Position (Top_Byte - 1);
            I := I + 1;
        end loop;

        exception
            when Apagerlib.Memories.No_Line_Found =>
                Top_Byte := 1;
    end Do_Page_Up;

    procedure Do_Previous_Line is
    begin
        Top_Byte := Buffer.Previous_Line_Position (Top_Byte - 1);

    exception
        when Apagerlib.Memories.No_Line_Found =>
            Top_Byte := 1;
    end Do_Previous_Line;

    procedure Quit_Handler is
    begin
        Run_Epilogue;
        raise End_Program_Exception;
    end Quit_Handler;

    procedure Read_Keyboard_Command is
    begin
        Keys := Apagerlib.Keyboard.Wait_For_Strkey;
        Command := Apagerlib.Commands.Keys_To_Command (Commands, Keys);

        if Command /= Apagerlib.Commands.Unknown_Command_String then
            return;
        end if;

        Ada.Strings.Unbounded.Append (Keys, " ");

        --  Print Keys
        Put (Ada.Strings.Unbounded.To_String (Keys));
        Ada.Strings.Unbounded.Append
            (Keys, Apagerlib.Keyboard.Wait_For_Strkey);
        Command := Apagerlib.Commands.Keys_To_Command (Commands, Keys);
    end Read_Keyboard_Command;

    procedure Run_Epilogue is
    begin
        Buffer.Close;
        Apagerlib.Keyboard.Close_Keyboard;
        Erase_Display (Entire_Screen);
        --  Need to do a scroll page up but, Scroll_Up does not work!
    end Run_Epilogue;

    procedure Show_Help is
        use Apagerlib.Commands.Command_Hashes;
        procedure Print_Keybinding (Position : Cursor);

        procedure Print_Keybinding (Position : Cursor) is
        begin
            Put_Line (To_String (Key (Position)) & " : "
                & To_String (Element (Position)));
        end Print_Keybinding;

        C : Character := ' ';
    begin
        Erase_Display (Entire_Screen);
        Cursor_Position (1, 1);
        Put_Line ("Keybinding help");
        Iterate (Commands, Print_Keybinding'Access);
        Put_Line ("Press q to return");

        while C /= 'q' and then C /= 'Q' loop
            C := Apagerlib.Keyboard.Wait_For_Key;
        end loop;
    end Show_Help;

    procedure Update_Minibuffer is
    begin
        Minibuffer.Width := Console.Geometry.Get_Columns - 1;
        Minibuffer.Height := 1;
        Minibuffer.Position_Column := 1;
        Minibuffer.Position_Line := Console.Geometry.Get_Lines - 1;
    end Update_Minibuffer;

    procedure Update_Modeline is
    begin
        Modeline.Line_Position := Console.Geometry.Get_Lines - 2;
        Modeline.Width := Console.Geometry.Get_Columns - 1;
        Modeline.Top_Byte := Top_Byte;
        Modeline.Truncate := (if Options.Truncate then Truncate
                              else Visual_Line);
    end Update_Modeline;

begin
    Assign_Input;

    Buffer.Open;
    Apagerlib.Keyboard.Open_Keyboard;
    Commands := Apagerlib.Commands.Default_Maps;

    GNAT.Ctrl_C.Install_Handler (Quit_Handler'Unrestricted_Access);

    while not Exit_Program loop
        Assign_Console_Size;

        Hide_Cursor;
        --  Go to the end of the screen... this creates a new space.
        Cursor_Position (Options.Lines, 1);

        Apagerlib.Frontend.Display.Print_Screen (Buffer.all,
                                                 Top_Byte, Options);

        Update_Modeline;
        Put_Modeline (Modeline);

        Update_Minibuffer;
        Minibuffer.Put_Minibuffer;
        --  Put (To_String (Command) & "(" & To_String (Keys) & ")");

        Show_Cursor;
        Read_Keyboard_Command;

        if Command = To_U ("execute-extended-command") then
            Minibuffer.Meta_X := True;
            Minibuffer.Put_Minibuffer;

            Command := Apagerlib.Keyboard.Get_Line;

            Minibuffer.Meta_X := False;
        end if;

        Exit_Program := Exit_Program or else Command = To_U ("quit");

        if Command = To_U ("previous-line") and then Top_Byte > 1 then
            Do_Previous_Line;
        end if;

        if Command = To_U ("next-line") then
            Do_Next_Line;
        end if;

        if Command = To_U ("scroll-up-command") then
            Do_Page_Down;
        end if;

        if Command = To_U ("scroll-down-command") then
            Do_Page_Up;
        end if;

        if Command = To_U ("end-of-buffer") then
            Top_Byte := Buffer.End_Position;
        end if;

        if Command = To_U ("beginning-of-buffer") then
            Top_Byte := 1;
        end if;

        if Command = To_U ("change-columns") then
            Minibuffer.Set_Message ("Column size?");
            Minibuffer.Put_Minibuffer;

            Command := Apagerlib.Keyboard.Get_Line;
            Options.Columns := Integer'Value (To_String (Command));
            Fixed_Size := True;

            Minibuffer.Reset;
        end if;

        if Command = To_U ("change-lines") then
            Minibuffer.Set_Message ("Lines size?");
            Minibuffer.Put_Minibuffer;

            Command := Apagerlib.Keyboard.Get_Line;
            Options.Lines := Integer'Value (To_String (Command));
            Fixed_Size := True;

            Minibuffer.Reset;
        end if;

        if Command = To_U ("truncate-mode") then
            Options.Truncate := not Options.Truncate;
        end if;

        if Command = To_U ("help")
            or else Command = To_U ("describe-bindings")
        then
            Show_Help;
        end if;

    end loop;

    Run_Epilogue;

    exception
        when End_Program_Exception =>
            null;
end Apager;
