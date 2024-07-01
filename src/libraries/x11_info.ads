--  x11_info.ads ---

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

with Interfaces.C;
use Interfaces.C;

package X11_Info is

    type Screensaver_Info_Type is record
        Invalid : Boolean;
        State : Integer;
        Kind : Integer;
        Til_Or_Since : unsigned_long;
        Idle : unsigned_long;
    end record;

    Invalid_Screensaver_Info : constant Screensaver_Info_Type := (
        Invalid => True,
        State => 0,
        Kind => 0,
        Til_Or_Since => 0,
        Idle => 0);

    function Screensaver_Default_Info return Screensaver_Info_Type;
    function Idle_Time_Default return unsigned_long
        is (Screensaver_Default_Info.Idle);

end X11_Info;
