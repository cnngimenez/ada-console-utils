--  widgets-countdowns.ads ---

--  Copyright 2022 cnngimenez
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

with Ada.Calendar;
use Ada.Calendar;
with Widgets.Labels;
use Widgets.Labels;

package Widgets.Countdowns is
    type Countdown_Type is new Label_Type with private;

    procedure Initialize (Widget : in out Countdown_Type;
                          To_Time : Time;
                          Row, Column : Natural);

    function Get_To_Time (Widget : Countdown_Type) return Time;
    procedure Set_To_Time (Widget : in out Countdown_Type; To_Time : Time);

    overriding procedure Draw (Widget : in out Countdown_Type);

private

    type Countdown_Type is new Label_Type with
    record
        To_Time : Time;
    end record;

    procedure Update_Difference (Widget : in out Countdown_Type);

end Widgets.Countdowns;
