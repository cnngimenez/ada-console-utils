--  countdown.adb ---

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

with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Calendar;
use Ada.Calendar;
with Ada.Calendar.Formatting;
use Ada.Calendar.Formatting;
with Ada.Calendar.Delays;
use Ada.Calendar.Delays;
with Widgets.Countdowns;
use Widgets.Countdowns;


procedure Countdown is
    Totime : Time;
begin
    if Argument_Count /= 1 then
        Put_Line ("Synopsis:");
        Put_Line ("    countdown ""YEAR-MONTH-DAY HOUR:MIN:SEC""");
        return;
    end if;

    Totime := Value (Argument (1));
    Widget.Initialize ("", 10, 10);

    loop
        Widget.Draw;
        Delay_For (Duration (1));
    end loop;

end Countdown;
