--  processes.ads ---

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

with Ada.Containers.Vectors;

--
--  Identify PID with comm and other basic processes subprograms.
--
--  Provide some basic functionality to identify and find system processes.
--  Other sublibraries are defined to parse specific Operative System
--  (Linux or BSD) process data and files.
--
--  Basic datatypes and file parsing are implemented here.  See sublibraries for
--  parsing specific files inside /proc/PID directory.
package Processes is

    subtype PID_Type is Natural;

    type Comm_String is new String (1 .. 16);
    --  A command string has a maximum of TASK_COMM_LEN (16) characters
    --  (including null byte).

    type Process_Type is record
        PID : PID_Type;
        Parent_PID : PID_Type;
        Command : Comm_String;
    end record;

    Invalid_Process : constant Process_Type := (
        PID => 0,
        Parent_PID => 0,
        Command => "Invalid         "
    );

    package Process_Vector_Pack is new Ada.Containers.Vectors (
        Index_Type => Positive,
        Element_Type => Process_Type
    );

    subtype Process_Vector is Process_Vector_Pack.Vector;

    function List_Processes return Process_Vector;
    --  Return all process Status.

    function Find_Process (Name_Substring : String; Owner : String)
        return Process_Type;
    --  Find the process with Name_Substring in its Name.
    --
    --  Search only among the Owner user.

    function Find_Process (Name_Substring : String)
        return Process_Type
    is (Find_Process (Name_Substring, ""));
    --  Find the process with Name_Substring in its Name.

    function Process_Stat (Stat_File_Path : String) return Process_Type;
    --  Read the process stat file and return its data.

    function Process_Stat (PID : PID_Type) return Process_Type;
    --  Find the process with the given PID and return its status.

    function Parse_Stat_String (S : String) return Process_Type;
end Processes;
