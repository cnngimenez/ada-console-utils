--  my_ps.adb ---

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

with Ada.Command_Line;
use Ada.Command_Line;
with Ada.Text_IO;
use Ada.Text_IO;
with Processes;
use Processes;
package Process.Stat;

procedure My_Ps is
    procedure Show_Help;
    procedure Show_Stat (Stat : Process_Type);
    procedure Show_One_Process;
    procedure Show_All_Processes;

    procedure Show_All_Processes is
        Process_Data : Process_Vector;
    begin
        Process_Data := List_Processes;
        for Process of Process_Data loop
            Show_Stat (Process);
        end loop;
    end Show_All_Processes;

    procedure Show_Help is
    begin
        Put_Line ("Show process information.");
        Put_Line ("Usage:");
        Put_Line ("    my_ps PID");
    end Show_Help;

    procedure Show_One_Process is
        use Processes.Stat;

        Stat : Stat_Type;
    begin
        Stat := Read_Stat (PID_Type'Value (Argument (1)));
        Show_Stat (Stat);
    end Show_One_Process;

    procedure Show_Stat (Stat : Stat_Type) is
        use Processes.Stat;
    begin
        Put_Line ("PID: " & Stat.PID'Image);
        Put_Line ("Command: " & Stat.Command);
        Put_Line ("State: " & Stat.State'Image);
        Put_Line ("Parent PID: " & Stat.Parent_PID'Image);
    end Show_Stat;

begin
    if Argument_Count < 1 then
        Show_All_Processes;
    elsif Argument_Count = 1 then
        Show_One_Process;
    else
        Show_Help;

    end if;
end My_Ps;
