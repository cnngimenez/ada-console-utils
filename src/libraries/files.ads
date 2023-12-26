--  files.ads ---

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

package Files is

    type File_Status_Type is record
        --  Device_ID :
        --  INode_Number :
        --  File_Type_Mode :
        --  Hard_Link_Count :
        User_ID : Positive;
        Group_ID : Positive;
        --  Size_Bytes :
        --  Block_Size :
        --  Block_512B_Allocated :
        --  Last_Access_Time :
        --  Last_Modification_Time :
        --  Last_Status_Change_Time :
    end record;

    function Get_File_Status (Path : String) return File_Status_Type;
    --  Return file Information.
    --
    --  It also works for Directories!

--  private
    --  procedure Stat (Pathname : String; Stat : Stat_Struct);
    --  pragma Import (C, Stat, "stat");

end Files;
