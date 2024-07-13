with Ada.Text_IO;
use Ada.Text_IO;

with Apagerlib.Keyboard;
with Apagerlib.Pages;
with Apagerlib.Display;

procedure Apager is

    Buffer : Apagerlib.Pages.Page_Type;
    C : Character := ' ';

begin
    Apagerlib.Keyboard.Open_Keyboard;

    while not End_Of_File and then C /= 'Q' loop
        Apagerlib.Pages.Get_Page (Buffer);
        Apagerlib.Display.Show_Page (Buffer);

        New_Line;
        Put_Line ("--  Page  --");
        C := Apagerlib.Keyboard.Wait_For_Key;
    end loop;

    Apagerlib.Keyboard.Close_Keyboard;
end Apager;
