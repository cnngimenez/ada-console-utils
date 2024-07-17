--  apagerlib-commands.adb ---

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

with Apagerlib.Keyboard;

package body Apagerlib.Commands is

    function Tou (Str : String) return Unbounded_String is
     (To_Unbounded_String (Str));

    function Default_Maps return Command_Map is
        Map : Command_Map;
    begin
        Map.Insert (Tou ("C-x C-c"), Tou ("quit"));
        Map.Insert (Tou ("M-x"), Tou ("execute-extended-command"));

        --  Navigation
        Map.Insert (Tou ("<up>"), Tou ("previous-line"));
        Map.Insert (Tou ("<down>"), Tou ("next-line"));
        Map.Insert (Tou ("<left>"), Tou ("left-char"));
        Map.Insert (Tou ("<right>"), Tou ("right-char"));

        Map.Insert (Tou ("h"), Tou ("left-char"));
        Map.Insert (Tou ("j"), Tou ("next-line"));
        Map.Insert (Tou ("k"), Tou ("previous-line"));
        Map.Insert (Tou ("l"), Tou ("right-Char"));

        --  Go to
        Map.Insert (Tou ("M-g g"), Tou ("goto-Line"));

        --  Help
        Map.Insert (Tou ("?"), Tou ("describe-bindings"));

        return Map;
    end Default_Maps;

    function Keys_To_Command (Map : Command_Map; Keys : Unbounded_String)
        return Unbounded_String is
        (if Map.Contains (Keys) then
            Map.Element (Keys)
        else
            Unknown_Command_String);

    function Wait_For_Command (Map : Command_Map) return Unbounded_String is
        Current_Key, Key : Unbounded_String;
    begin
        Current_Key := Apagerlib.Keyboard.Wait_For_Strkey;

        if Map.Contains (Current_Key) then
            return Map.Element (Current_Key);
        end if;

        Key := Current_Key;
        Current_Key := Apagerlib.Keyboard.Wait_For_Strkey;

        if Map.Contains (Key) then
            return Map.Element (Key);
        else
            return Unknown_Command_String;
        end if;

    end Wait_For_Command;

end Apagerlib.Commands;
