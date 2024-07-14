with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

with Apagerlib.Keyboard;
with Apagerlib.Pages;
with Apagerlib.Display;
with Apagerlib.Commands;

procedure Apager is

    procedure Read_Keyboard_Command;

    Buffer : Apagerlib.Pages.Page_Type;
    Commands : Apagerlib.Commands.Command_Map;
    Command : Unbounded_String;
    Exit_Program : Boolean := False;

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
    Apagerlib.Keyboard.Open_Keyboard;
    Commands := Apagerlib.Commands.Default_Maps;

    while not End_Of_File and then not Exit_Program
    loop
        Apagerlib.Pages.Get_Page (Buffer);
        Apagerlib.Display.Show_Page (Buffer);

        New_Line;
        Put_Line ("--  Page  --");
        Read_Keyboard_Command;

    end loop;

    Apagerlib.Keyboard.Close_Keyboard;
end Apager;
