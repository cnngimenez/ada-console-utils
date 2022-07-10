--  widgets-countdowns.adb ---

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
with Ada.Calendar.Arithmetic;
use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
use Ada.Calendar.Formatting;
with Ada.Calendar.Delays;
use Ada.Calendar.Delays;


package body Widgets.Countdowns is

    procedure Update_Difference (Widget : in out Countdown_Type); is
        Now : constant Ada.Calendar.Time := Clock;
        Days : Day_Count;
        seconds : Duration;
        Leap_Seconds : Leap_Seconds_Count;
        Hours, Minutes, Toseconds : Natural;
    begin
        Difference (Totime, Now, Days, Seconds, Leap_Seconds);

        --  Seconds has the remainding seconds within a day.
        Hours := Natural (Seconds) / 60 / 60;
        Minutes := Natural (Seconds) / 60 - Hours * 60;
        Toseconds := Natural (Seconds) mod 60;

        --  --  Debugging purposes only:
        --  Put_Line (Days'Image  & "d - "
        --      & Hours'Image & "h - "
        --      & Minutes'Image & "m - "
        --      & Toseconds'Image & "s");
    end Update_Difference;
end Widgets.Countdowns;
