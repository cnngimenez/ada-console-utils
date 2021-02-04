--  emojis.ads ---

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

with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;

package Emojis is

    --  Emoji components
    --  Regional_Indicator is ...

    --  Some emojis like faces, hands and persons allows to change the
    --  skin tone. Use this and Get_Skin function to add one of the possible
    --  skins.
    type Skin_Type is (Light, Medium_Light, Medium, Medium_Dark, Dark);

    --  Same as Skin_Type. Change the hear style with Get_Hair function.
    type Hair_Type is (Red_Hair, Curly_Hair, Bald, White_Hair);

    --  Some fonts and services allows to use two or more emojis to create one.
    --  For example, joining a speech bubble + zero_width_joiner + eye creates
    --  a speech bubble with an eye inside.
    Zero_Width_Joiner : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (16#200D#);

    --  Shows the simple character that is before as a keyboard key.
    Combining_Enclosing_Keycap : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (16#20E3#);

    --  Variation selector allows to choose the imaged emoji instead the simple
    --  monospaced font.
    Variation_Selector : constant Wide_Wide_Character :=
      Wide_Wide_Character'Val (16#FE0F#);

    function Get_Skin (Skin : Skin_Type) return Wide_Wide_Character;
    function Get_Skin (Emoji : Wide_Wide_Character;
                       Skin : Skin_Type) return Wide_Wide_String;
    function Get_Hair (Hair : Hair_Type) return Wide_Wide_Character;
    function Get_Hair (Emoji : Wide_Wide_Character;
                       Hair : Hair_Type) return Wide_Wide_String;

    function Get_Keycap (Char : Wide_Wide_Character)
                        return Wide_Wide_String;
    function Get_Emoji_Variation (Char : Wide_Wide_Character)
                                 return Wide_Wide_String;

    --  Add a Zero Width Joiner (ZWJ) character between the input string's
    --  characters.
    function Join_With_Zwj (Characters : Wide_Wide_String)
                           return Unbounded_Wide_Wide_String;
    function Join_With_Zwj (Characters : Wide_Wide_String)
                           return Wide_Wide_String;
    --
    --  Printing functions.
    --

    procedure Put_Skin (Skin : Skin_Type);
    procedure Put_Skin (Emoji : Wide_Wide_Character; Skin : Skin_Type);
    procedure Put_Hair (Hair : Hair_Type);
    procedure Put_Hair (Emoji : Wide_Wide_Character; Hair : Hair_Type);

    --  Put the given emoji character.
    procedure Put_Emoji (Number : Positive);

    --  Put the Zero Width Joiner to standard IO
    procedure Put_Zwj;

    --  Put the Variation Selector to standard IO
    procedure Put_Variation;

    --  Put to standard IO the Combining Enclosing Keycap
    procedure Put_Keycap;

    procedure Put_Person;

end Emojis;
