--  emojis.adb ---

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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

with Ada.Wide_Wide_Text_IO;
use Ada.Wide_Wide_Text_IO;

package body Emojis is

    function Get_Emoji_Variation (Char : Wide_Wide_Character)
                                 return Wide_Wide_String
    is
    begin
        return Char & Variation_Selector;
    end Get_Emoji_Variation;

    function Get_Hair (Hair : Hair_Type) return Wide_Wide_Character is
    begin
        return Wide_Wide_Character'Val
          (Hair_Type'Pos (Hair) + 16#1F9B0#);
    end Get_Hair;

    function Get_Hair (Emoji : Wide_Wide_Character;
                       Hair : Hair_Type)
                      return Wide_Wide_String
    is
    begin
        return Emoji & Wide_Wide_Character'Val
          (Hair_Type'Pos (Hair) + 16#1f9b0#);
    end Get_Hair;

    function Get_Keycap (Char : Wide_Wide_Character)
                        return Wide_Wide_String
    is
    begin
        return Char & Combining_Enclosing_Keycap;
    end Get_Keycap;

    function Get_Skin (Skin : Skin_Type) return Wide_Wide_Character is
    begin
        return Wide_Wide_Character'Val
          (Skin_Type'Pos (Skin) + 16#1F3FB#);
    end Get_Skin;

    function Get_Skin (Emoji : Wide_Wide_Character;
                       Skin : Skin_Type)
                      return Wide_Wide_String
    is
    begin
        return Emoji & Wide_Wide_Character'Val
          (Skin_Type'Pos (Skin) + 16#1f3fb#);
    end Get_Skin;

    function Join_With_Zwj (Characters : Wide_Wide_String)
                           return Unbounded_Wide_Wide_String
    is
        Str_Output : Unbounded_Wide_Wide_String;
        Char : Wide_Wide_Character;
    begin
        for I in Characters'Range loop
            Char := Characters (I);
            Append (Str_Output, Char);
            Append (Str_Output, Zero_Width_Joiner);
        end loop;

        return Str_Output;
    end Join_With_Zwj;

    function Join_With_Zwj (Characters : Wide_Wide_String)
                           return Wide_Wide_String
    is
        Unbounded_Str : Unbounded_Wide_Wide_String;
    begin
        Unbounded_Str := Join_With_Zwj (Characters);

        return To_Wide_Wide_String (Unbounded_Str);
    end Join_With_Zwj;

    procedure Put_Emoji (Number : Positive) is
    begin
        Put (Wide_Wide_Character'Val (Number));
    end Put_Emoji;

    procedure Put_Hair (Hair : Hair_Type) is
    begin
        Put (Get_Hair (Hair));
    end Put_Hair;

    procedure Put_Hair (Emoji : Wide_Wide_Character; Hair : Hair_Type) is
    begin
        Put (Get_Hair (Emoji, Hair));
    end Put_Hair;

    procedure Put_Keycap is
    begin
        Put (Combining_Enclosing_Keycap);
    end Put_Keycap;

    procedure Put_Person is
    begin
        Put (Wide_Wide_Character'Val (16#1F9D1#));
    end Put_Person;

    procedure Put_Skin (Skin : Skin_Type) is
    begin
        Put (Get_Skin (Skin));
    end Put_Skin;

    procedure Put_Skin (Emoji : Wide_Wide_Character; Skin : Skin_Type) is
    begin
        Put (Get_Skin (Emoji, Skin));
    end Put_Skin;

    procedure Put_Variation is
    begin
        Put (Variation_Selector);
    end Put_Variation;

    procedure Put_Zwj is
    begin
        Put (Zero_Width_Joiner);
    end Put_Zwj;

end Emojis;
