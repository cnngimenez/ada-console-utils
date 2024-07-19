with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Console.CSI_Codes;
use Console.CSI_Codes;
with Console.SGR;
with Console.Geometry;
with Apagerlib.Keyboard;
with Apagerlib.Memories;
with Apagerlib.Display;
with Apagerlib.Commands;

procedure Apager is

    function To_U (Item : String) return Unbounded_String
        renames To_Unbounded_String;

    procedure Read_Keyboard_Command;
    procedure Show_Help;

    Buffer : Apagerlib.Memories.Page_Memory;
    Commands : Apagerlib.Commands.Command_Map;
    Command : Unbounded_String;
    Exit_Program : Boolean := False;
    Top_Byte : Positive := 1;
    Options : Apagerlib.Display.Display_Options;

    procedure Read_Keyboard_Command is
        Keys : Unbounded_String;
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

begin
    Buffer.Initialise;
    Apagerlib.Keyboard.Open_Keyboard;
    Commands := Apagerlib.Commands.Default_Maps;
    Options.Columns := Console.Geometry.Get_Columns;
    Options.Lines := Console.Geometry.Get_Lines - 3;

    while not Exit_Program loop
        Erase_Display (Entire_Screen);
        Cursor_Position (1, 1);
        Apagerlib.Display.Print_Screen (Buffer, Top_Byte, Options);

        New_Line;
        Console.SGR.Reverse_Video;
        Put_Line (Top_Byte'Image & " " & Buffer.Last_Loaded_Page'Image & " "
            & Options.Columns'Image & "x" & Options.Lines'Image
            & (if Options.Truncate then "-T-" else "-\-"));
        Console.SGR.Reset_All;
        Put (To_String (Command));
        Read_Keyboard_Command;

        if Command = To_U ("execute-extended-command") then
            Put ("M-x ");
            Command := Apagerlib.Keyboard.Get_Line;
        end if;

        Exit_Program := Command = To_U ("quit");

        if Command = To_U ("previous-line") and then Top_Byte > 1 then
            Top_Byte := Apagerlib.Memories.Previous_Line_Byte
                (Buffer, Top_Byte - 1);
        end if;

        if Command = To_U ("next-line") then
            Top_Byte :=
                Apagerlib.Memories.Next_Line_Byte (Buffer, Top_Byte + 1);
        end if;

        if Command = To_U ("change-columns") then
            Put ("Column size?");
            Command := Apagerlib.Keyboard.Get_Line;
            Options.Columns := Integer'Value (To_String (Command));
        end if;

        if Command = To_U ("change-lines") then
            Put ("Lines size?");
            Command := Apagerlib.Keyboard.Get_Line;
            Options.Lines := Integer'Value (To_String (Command));
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

    Apagerlib.Keyboard.Close_Keyboard;
end Apager;
