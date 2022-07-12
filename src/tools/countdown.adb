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
with Widgets.Countdowns;
use Widgets.Countdowns;
with Widgets.Times;
use Widgets.Times;
with Console;

procedure Countdown is
    Totime : Time;
    Totime_Widget, Time_Widget : Time_Type;
    Widget : Countdown_Type;
begin
    if Argument_Count /= 1 then
        Put_Line ("Synopsis:");
        Put_Line ("    countdown ""YEAR-MONTH-DAY HOUR:MIN:SEC""");
        return;
    end if;

    Totime := Value (Argument (1));
    Widget.Initialize ("Duration: ", Totime, 10, 10);

    Totime_Widget.Initialize ("To: ", 6, 10);
    Totime_Widget.Set_Time (Totime);
    Time_Widget.Initialize ("Current: ", 5, 10);

    Console.Erase_Display (Console.Entire_Screen);
    loop
        Totime_Widget.Draw;
        Time_Widget.Draw;
        Widget.Draw;
        delay 1.0;
    end loop;

end Countdown;
