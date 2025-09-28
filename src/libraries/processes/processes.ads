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

    Proc_Path : constant String := "/proc";

    subtype PID_Type is Natural;

    function Process_Path (PID : PID_Type) return String;
    --  Return the path from the PID of a process.
    --
    --  PID or path may not exists.

    type Comm_String is new String (1 .. 16);
    --  A command string has a maximum of TASK_COMM_LEN (16) characters
    --  (including null byte).

    Empty_Comm_String : constant Comm_String := "               ";

    type Process_Type is record
        PID : PID_Type;
        Command : Comm_String;
    end record;

    Invalid_Process : constant Process_Type := (
        PID => 0,
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

    function Find_Process (PID : PID_Type) return Process_Type;

    function Comm_Path (PID : PID_Type) return String
        is (Process_Path (PID) & "/comm");

    function Read_Comm (PID : PID_Type) return Comm_String;
    --  Read the comm file of the given PID process.
    --
    --  Returns an Empty_Comm_String if PID does not exists.

end Processes;
