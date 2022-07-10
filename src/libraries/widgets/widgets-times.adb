--  widgets-times.adb ---

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

package body Widgets.Times is
    procedure Initialize (Widget : in out Time_Type;
                          Text : Wide_Wide_String;
                          Row, Column : Natural) is
    begin 
        --  Height is 8 because the length of "24:00:00".
        Widgets.Labels.Initialize (Label_Type (Widget), Text,
                                   Row, Column, 1, 8 + Text'Length);
    end Initialize;

    overriding procedure Draw (Time : in out Time_Type); is 
    begin
        
    end Draw;
end Widgets.Times;
