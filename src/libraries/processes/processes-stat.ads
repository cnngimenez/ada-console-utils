--  processes-stat.ads ---

--  Copyright 2025 cnngimenez
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
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.

-------------------------------------------------------------------------

--  Parse the process stat file.
--
--  Stat files are files with information about the current status of a process.
--  They are located at /proc/PID/stat file.  Also, the /proc/PID/status file
--  has a more human readable format, but in this library the stat file is used.
--
--  The proc_pid_stat(5) manpage explains the file format and its fields.  This
--  library defines several types and structures explained there.
package Processes.Stat is

    type Comm_String is new String (1 .. 16);
    --  A command string has a maximum of TASK_COMM_LEN (16) characters
    --  (including null byte).

    type Stat_Type is tagged record
        Pid : PID_Type;
        Comm : Comm_String;
        State : State_Type;
        Ppid : PID_Type;
        Pgrp : Integer;
        Session : Integer;
        Tty_Nr : Integer;
        Tpgid : Integer;
        Flags : Natural;
        Minflt : Natural;
        Cminflt : Natural;
        Majflt : Natural;
        Cmajflt : Natural;
        Utime : Natural;
        Stime : Natural;
        Cutime : Integer;
        Cstime : Integer;
        Priority : Integer;
        Nice : Integer;
        Num_Threads : Integer;
        Itrealvalue : Integer;
        Starttime : Natural;
        Vsize : Natural;
        Rss : Integer;
        Rsslim : Natural;
        Startcode : Natural;
        Endcode : Natural;
        Stratstack : Natural;
        Kstkesp : Natural;
        Kstkeip  : Natural;
        Signal : Natural;
        Blocked : Natural;
        Sigignore : Natural;
        Sigcatch : Natural;
        Wchan : Natural;
        Nswap : Natural;
        Cnswap : Natural;
        Exit_Signal : Integer;
        Processor : Integer;
        Rt_Priority : Natural;
        Policy : Natural;
        Delayacct_Blkio_Ticks : Natural;
        Guest_Time : Natural;
        Cguest_Time : Integer;
        Start_Data : Natural;
        End_Data : Natural;
        Start_Brk : Natural;
        Arg_Start : Natural;
        Arg_End : Natural;
        Env_Start : Natural;
        Env_End : Natural;
        Exit_Code : Integer;
    end record;
    --  This is the structure of a PID stat file.
    --  It is explained at man 5 proc_pid_stat. The names of the record is the
    --  same as the ones appearing in the manpage. Alias functions are used to
    --  provide more readability.

    --  --------------------------------------------------
    --  Stat_Type field aliases.
    --  --------------------------------------------------

    function Parent_PID (S : Stat_Type) return Natural
        is (S.Ppid);

    function User_Time (S : Stat_Type) return Natural
        is (S.Utime);

    function System_Time (S : Stat_Type) return Natural
        is (S.Stime);

    --  --------------------------------------------------
    --  Reading stat files
    --  --------------------------------------------------

    function Stat_Path (PID : PID_Type) return String;
    --  Path to the stat file of the given process.

    function Read_Stat (Path : String) return Stat_Type;
    --  Read a stat file by providing its path.

    function Read_Stat (PID : PID_Type) return Stat_Type
        is Read_Stat (Stat_Path (PID));
    --  Read a PID stat file from /proc/PID/stat


end Processes.Stat;
