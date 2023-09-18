--  widgets-labels.adb ---

--  Copyright 2020 cnngimenez
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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

--  with Console;
--  use Console;

package body Widgets.Labels is

    overriding procedure Draw (Label : in out Label_Type) is
    begin
        Widgets.Draw (Widget_Type (Label));
        --  Cursor_Position (Label.Row, Label.Column);

        Put_Line (To_Wide_Wide_String (Label.Text));
    end Draw;

    function Get_Text (Label : Label_Type) return Unbounded_Wide_Wide_String is
      (Label.Text);

    function Get_Text (Label : Label_Type) return Wide_Wide_String is
      (To_Wide_Wide_String (Label.Text));

    procedure Initialize (Label : in out Label_Type;
                          Text : Wide_Wide_String;
                          Row, Column, Width, Height : Natural) is
    begin
        Label.Initialize (To_Unbounded_Wide_Wide_String (Text),
                          Row, Column, Width, Height);
    end Initialize;
    procedure Initialize (Label : in out Label_Type;
                          Text : Unbounded_Wide_Wide_String;
                          Row, Column, Width, Height : Natural) is
    begin
        Widgets.Initialize (Widget_Type (Label),
                            Row, Column, Width, Height);
        Label.Text := Text;
    end Initialize;

    procedure Set_Text (Label : in out Label_Type;
                        Text : Unbounded_Wide_Wide_String) is
    begin
        Label.Text := Text;
        --  Label.Draw;
    end Set_Text;

    procedure Set_Text (Label : in out Label_Type;
                        Text : Wide_Wide_String) is
    begin
        Label.Set_Text (To_Unbounded_Wide_Wide_String (Text));
    end Set_Text;

end Widgets.Labels;
