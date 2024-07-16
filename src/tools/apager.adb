with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Console.CSI_Codes;
use Console.CSI_Codes;
with Console.SGR;
with Apagerlib.Keyboard;
with Apagerlib.Pages;
with Apagerlib.Display;
with Apagerlib.Commands;

procedure Apager is

    function To_U (Item : String) return Unbounded_String
        renames To_Unbounded_String;

    procedure Read_Keyboard_Command;

    Buffer : Apagerlib.Pages.Page_Memory;
    Commands : Apagerlib.Commands.Command_Map;
    Command : Unbounded_String;
    Exit_Program : Boolean := False;
    Top_Byte : Positive := 1;

    procedure Read_Keyboard_Command is
        Keys : Unbounded_String;
    begin
        Keys := Apagerlib.Keyboard.Wait_For_Strkey;
        Command := Apagerlib.Commands.Keys_To_Command (Commands, Keys);

        if Command /= Apagerlib.Commands.Unknown_Command_String then
            return;
        end if;

        --  Print Keys
        Put (Ada.Strings.Unbounded.To_String (Keys));
        Ada.Strings.Unbounded.Append
            (Keys, Apagerlib.Keyboard.Wait_For_Strkey);
        Command := Apagerlib.Commands.Keys_To_Command (Commands, Keys);
    end Read_Keyboard_Command;

begin
    Buffer.Initialise;
    Apagerlib.Keyboard.Open_Keyboard;
    Commands := Apagerlib.Commands.Default_Maps;

    while not End_Of_File and then not Exit_Program
    loop
        Erase_Display (Entire_Screen);
        Cursor_Position (1, 1);
        Apagerlib.Display.Print_Screen (Buffer, Top_Byte);

        New_Line;
        Console.SGR.Reverse_Video;
        Put_Line (Top_Byte'Image & " " & Buffer.Last_Loaded_Page'Image);
        Console.SGR.Reset_All;
        Put (To_String (Command));
        Read_Keyboard_Command;

        if Command = To_U ("execute-extended-command") then
            Put ("M-x ");
            Command := Apagerlib.Keyboard.Get_Line;
        end if;

        Exit_Program := Command = To_U ("Quit");

        if Command = To_U ("previous-line") and then Top_Byte > 1 then
            Top_Byte := Apagerlib.Pages.Previous_Line_Byte
                (Buffer, Top_Byte - 1);
        end if;

        if Command = To_U ("next-line") then
            Top_Byte := Apagerlib.Pages.Next_Line_Byte (Buffer, Top_Byte + 1);
        end if;
    end loop;

    Apagerlib.Keyboard.Close_Keyboard;
end Apager;
