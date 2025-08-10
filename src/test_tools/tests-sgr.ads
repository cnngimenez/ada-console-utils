--  tests-sgr.ads ---

--  Copyright 2023 cnngimenez
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

package Tests.SGR is

    procedure Alternative_Font_Test;

    procedure Set_Colour_Test;

    procedure Bright_Colour_Test;

    procedure Set_Background_Test;

    procedure Bright_Background_Test;

    procedure Set_RGB_Test;

    procedure Set_RGB_Background_Test;

    procedure Set_Colour_8bit_Test;

    procedure Set_Background_8bit_Test;

    procedure Test_Codes_0_To_10;
    procedure Test_Codes_10_To_20;
    procedure Test_Codes_20_To_30;
    procedure Test_Codes_30_To_40;
    procedure Test_Codes_40_To_50;
    procedure Test_Codes_50_To_60;
    procedure Test_Codes_60_To_70;
    procedure Test_Codes_90_To_110;

private

    procedure Write (Str : String);

end Tests.SGR;
