--  files.adb ---

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

with Ada.Calendar.Conversions;
with Interfaces.C;

package body Files is

    type Stat_Struct is record
        User_Id : Interfaces.C.int;
        Group_Id : Interfaces.C.int;
        Access_Time : Interfaces.C.long;
        Modification_Time : Interfaces.C.long;
        Status_Change_Time : Interfaces.C.long;
    end record;
    pragma Convention (C, Stat_Struct);

    procedure My_Stat (Path : String; Stat : in out Stat_Struct);
    pragma Import (C, My_Stat, "my_stat");

    function Get_File_Status (Path : String) return File_Status_Type is
        use Ada.Calendar.Conversions;

        File_Status : File_Status_Type;
        Stat : Stat_Struct;
    begin
        My_Stat (Path, Stat);

        File_Status.User_ID := Positive (Stat.User_Id);
        File_Status.Group_ID := Positive (Stat.Group_Id);

        File_Status.Last_Access_Time := To_Ada_Time (Stat.Access_Time);
        File_Status.Last_Modification_Time :=
            To_Ada_Time (Stat.Modification_Time);
        File_Status.Last_Status_Change_Time :=
            To_Ada_Time (Stat.Status_Change_Time);

        return File_Status;
    end Get_File_Status;

end Files;
