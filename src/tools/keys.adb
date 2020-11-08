with Ada.Text_IO;
use Ada.Text_IO;

procedure Keys is
    Key : Character;
begin
    loop
        Get_Immediate (Key);

        Put_Line (Positive'Image (Character'Pos (Key)));
    end loop;
end Keys;
