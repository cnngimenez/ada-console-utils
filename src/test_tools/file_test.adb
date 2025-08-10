--  file_test.adb ---

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
--  along with this program.  If not, see <http://www.gnu.org/Licenses/>.

-------------------------------------------------------------------------

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Calendar.Formatting;

with Files;

procedure File_Test is
    File_Status : Files.File_Status_Type;
begin
    File_Status := Files.Get_File_Status (Argument (1));

    Put_Line ("File: " & Argument (1));
    Put_Line ("User ID: " & File_Status.User_ID'Image);
    Put_Line ("Group ID: " & File_Status.Group_ID'Image);

    Put_Line ("Access time: "
        & Ada.Calendar.Formatting.Image (File_Status.Last_Access_Time));
    Put_Line ("Modification time: "
        & Ada.Calendar.Formatting.Image (File_Status.Last_Modification_Time));
    Put_Line ("Status Change time: "
        & Ada.Calendar.Formatting.Image (File_Status.Last_Status_Change_Time));
end File_Test;
