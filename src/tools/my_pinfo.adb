--  my_pinfo.adb ---

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

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Command_Line;
use Ada.Command_Line;
with Processes;
use Processes;
with Processes.Stats;
use Processes.Stats;
with Processes.Stats.PF_Flags;

procedure My_Pinfo is
    Stat : Stat_Type;

    procedure Print_Flags;
    procedure Print_Ownership;
    procedure Print_Times;

    procedure Print_Flags is
        use Processes.Stats.PF_Flags;
        Flags : constant Flag_Type := Stat_Flags (Stat);
    begin
        Put_Line ("* Flags");
        Put_Line ("Flags    : " & Stat.Flags'Image);
        if Flags.Virtual_CPU then
            Put ("Virtual_CPU ");
        end if;
        if Flags.IDLE_Thread then
            Put ("IDLE_Thread ");
        end if;
        if Flags.Exiting then
            Put ("Exiting ");
        end if;
        if Flags.Postcoredump then
            Put ("Postcoredump ");
        end if;
        if Flags.IO_Worker then
            Put ("IO_Worker ");
        end if;
        if Flags.Workqueue_Worker then
            Put ("Workqueue_Worker ");
        end if;
        if Flags.Fork_No_Exec then
            Put ("Fork_No_Exec ");
        end if;
        if Flags.MCE_Process then
            Put ("MCE_Process ");
        end if;
        if Flags.Superuser_Privs then
            Put ("Superuser_Privs ");
        end if;
        if Flags.Dumped_Core then
            Put ("Dumped_Core ");
        end if;
        if Flags.Signaled then
            Put ("Signaled ");
        end if;
        if Flags.Memory_Allocating then
            Put ("Memory_Allocating");
        end if;
        New_Line;
    end Print_Flags;

    procedure Print_Ownership is
    begin
        Put_Line ("* Ownership");
        Put_Line ("Login UID: "
            & Read_Loginuid (Stat.Pid)'Image);
    end Print_Ownership;

    procedure Print_Times is
    begin
        Put_Line ("* Times");
        Put_Line ("Utime    : " & Stat.Utime'Image);
        Put_Line ("Stime    : " & Stat.Stime'Image);
        Put_Line ("Cutime   : " & Stat.Cutime'Image);
        Put_Line ("Cstime   : " & Stat.Cstime'Image);
        Put_Line ("Starttime : " & Stat.Starttime'Image);
        Put_Line ("Guest_Time : " & Stat.Guest_Time'Image);
        Put_Line ("Cguest_Time : " & Stat.Cguest_Time'Image);
    end Print_Times;

begin
    Stat := Read_Stat (PID_Type'Value (Argument (1)));

    Put_Line ("* Process information");
    Put_Line ("PID      : " & Stat.Pid'Image);
    Put_Line ("Command  : " & String (Stat.Comm));
    Put_Line ("Command Line : "
        & Read_Command_Line (Stat.Pid));

    Print_Flags;
    Print_Ownership;
    Print_Times;

    Put_Line ("* Process state");
    Put_Line ("State    : " & Stat.State'Image);
    Put_Line ("Ppid     : " & Stat.Ppid'Image);
    Put_Line ("Pgrp     : " & Stat.Pgrp'Image);
    Put_Line ("Session  : " & Stat.Session'Image);
    Put_Line ("Tty_Nr   : " & Stat.Tty_Nr'Image);
    Put_Line ("Tpgid    : " & Stat.Tpgid'Image);

    Put_Line ("Minflt   : " & Stat.Minflt'Image);
    Put_Line ("Cminflt  : " & Stat.Cminflt'Image);
    Put_Line ("Majflt   : " & Stat.Majflt'Image);
    Put_Line ("Cmajflt  : " & Stat.Cmajflt'Image);
    Put_Line ("Priority : " & Stat.Priority'Image);
    Put_Line ("Nice     : " & Stat.Nice'Image);
    Put_Line ("Num_Threads : " & Stat.Num_Threads'Image);
    Put_Line ("Itrealvalue : " & Stat.Itrealvalue'Image);
    Put_Line ("Vsize   : " & Stat.Vsize'Image);
    Put_Line ("Rss     : " & Stat.Rss'Image);
    Put_Line ("Rsslim  : " & Stat.Rsslim'Image);
    Put_Line ("Startcode : " & Stat.Startcode'Image);
    Put_Line ("Endcode : " & Stat.Endcode'Image);
    Put_Line ("Stratstack : " & Stat.Stratstack'Image);
    Put_Line ("Kstkesp : " & Stat.Kstkesp'Image);
    Put_Line ("Kstkeip : " & Stat.Kstkeip'Image);
    Put_Line ("Signal  : " & Stat.Signal'Image);
    Put_Line ("Blocked : " & Stat.Blocked'Image);
    Put_Line ("Sigignore : " & Stat.Sigignore'Image);
    Put_Line ("Sigcatch : " & Stat.Sigcatch'Image);
    Put_Line ("Wchan   : " & Stat.Wchan'Image);
    Put_Line ("Nswap   : " & Stat.Nswap'Image);
    Put_Line ("Cnswap  : " & Stat.Cnswap'Image);
    Put_Line ("Exit_Signal : " & Stat.Exit_Signal'Image);
    Put_Line ("Processor : " & Stat.Processor'Image);
    Put_Line ("Rt_Priority : " & Stat.Rt_Priority'Image);
    Put_Line ("Policy  : " & Stat.Policy'Image);
    Put_Line ("Delayacct_Blkio_Ticks : " & Stat.Delayacct_Blkio_Ticks'Image);
    Put_Line ("Start_Data : " & Stat.Start_Data'Image);
    Put_Line ("End_Data : " & Stat.End_Data'Image);
    Put_Line ("Start_Brk : " & Stat.Start_Brk'Image);
    Put_Line ("Arg_Start : " & Stat.Arg_Start'Image);
    Put_Line ("Arg_End : " & Stat.Arg_End'Image);
    Put_Line ("Env_Start : " & Stat.Env_Start'Image);
    Put_Line ("Env_End : " & Stat.Env_End'Image);
    Put_Line ("Exit_Code : " & Stat.Exit_Code'Image);
end My_Pinfo;
