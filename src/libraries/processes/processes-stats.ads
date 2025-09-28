--  processes-stats.ads ---

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
--  Stat files are files with information about the current status of a
--  process.  They are located at /proc/PID/stat file.  Also, the
--  /proc/PID/status file has a more human readable format, but in this
--  library the stat file is used.
--
--  The proc_pid_stat(5) manpage explains the file format and its fields.  This
--  library defines several types and structures explained there.
package Processes.Stats is
    type State_Type is (Running, Sleeping, Waiting_Disk, Zombie,
        Stopped_On_Signal, Tracing_Stop, Paging, Dead, Wakekill,
        Waking, Parked, Idle, Not_Recognised);

    type Unsigned is mod 2 ** Integer'Size;
    --  Unsigned types for Stat_Type.
    --
    --  Natural numbers represented as modular types.
    --
    --  Unsigned are represented as %u and must be modular in order to use
    --  bitwise operations. Natural and Integer are ranged types, and therefore
    --  may support negative numbers which does not support bitwise operations.

    function To_Character (State : State_Type) return Character;

    function To_String (State : State_Type) return String
        is (To_Character (State)'Image);

    function State_String_To_Type (S : String) return State_Type;
    --  Convert process state String to State_Type.
    --
    --  See proc_pid_stat(5) manpage for state character meanings.

    function State_String_To_Type (C : Character) return State_Type;
    --  Convert process state Character to State_Type.
    --
    --  See proc_pid_stat(5) manpage for state character meanings.

    type Stat_Type is tagged record
        Pid : PID_Type;
        Comm : Comm_String;
        State : State_Type;
        Ppid : PID_Type;
        Pgrp : Integer;
        Session : Integer;
        Tty_Nr : Integer;
        Tpgid : Integer;
        Flags : Unsigned;
        Minflt : Unsigned;
        Cminflt : Unsigned;
        Majflt : Unsigned;
        Cmajflt : Unsigned;
        Utime : Unsigned;
        Stime : Unsigned;
        Cutime : Integer;
        Cstime : Integer;
        Priority : Integer;
        Nice : Integer;
        Num_Threads : Integer;
        Itrealvalue : Integer;
        Starttime : Unsigned;
        Vsize : Unsigned;
        Rss : Integer;
        Rsslim : Unsigned;
        Startcode : Unsigned;
        Endcode : Unsigned;
        Stratstack : Unsigned;
        Kstkesp : Unsigned;
        Kstkeip  : Unsigned;
        Signal : Unsigned;
        Blocked : Unsigned;
        Sigignore : Unsigned;
        Sigcatch : Unsigned;
        Wchan : Unsigned;
        Nswap : Unsigned;
        Cnswap : Unsigned;
        Exit_Signal : Integer;
        Processor : Integer;
        Rt_Priority : Unsigned;
        Policy : Unsigned;
        Delayacct_Blkio_Ticks : Unsigned;
        Guest_Time : Unsigned;
        Cguest_Time : Integer;
        Start_Data : Unsigned;
        End_Data : Unsigned;
        Start_Brk : Unsigned;
        Arg_Start : Unsigned;
        Arg_End : Unsigned;
        Env_Start : Unsigned;
        Env_End : Unsigned;
        Exit_Code : Integer;
    end record;
    --  This is the structure of a PID stat file.
    --  It is explained at man 5 proc_pid_stat. The names of the record is the
    --  same as the ones appearing in the manpage. Alias functions are used to
    --  provide more readability.

    --  --------------------------------------------------
    --  Stat_Type field aliases.
    --  --------------------------------------------------

    function Parent_PID (S : Stat_Type) return PID_Type
        is (S.Ppid);

    function User_Time (S : Stat_Type) return Unsigned
        is (S.Utime);

    function System_Time (S : Stat_Type) return Unsigned
        is (S.Stime);

    function State_String (S : Stat_Type) return Character
        is (To_Character (S.State));

    function State_String (S : Stat_Type) return String
        is (To_String (S.State));

    --  --------------------------------------------------
    --  Reading stat files
    --  --------------------------------------------------

    function Stat_Path (PID : PID_Type) return String;
    --  Path to the stat file of the given process.

    function Read_Stat (Path : String) return Stat_Type;
    --  Read a stat file by providing its path.

    function Read_Stat (PID : PID_Type) return Stat_Type
        is (Read_Stat (Stat_Path (PID)));
    --  Read a PID stat file from /proc/PID/stat

end Processes.Stats;
