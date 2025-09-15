--  processes-stat-pf_flags.adb ---

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

package body Processes.Stat.PF_Flags is

    function  Stat_Flags (Stat : Stat_Type) return Flags_Type is
        Flag : Flags_Type;
    begin
        Flag.Virtual_Cpu       := Stat.Flags and 0x01 > 0;
        Flag.IDLE_Thread       := Stat.Flags and 0x02 > 0 ;
        Flag.Exiting           := Stat.Flags and 0x04 > 0 ;
        Flag.Postcoredump      := Stat.Flags and 0x08 > 0 ;
        Flag.IO_Worker         := Stat.Flags and 0x10 > 0 ;
        Flag.Workqueue_Worker  := Stat.Flags and 0x20 > 0 ;
        Flag.Fork_No_Exec      := Stat.Flags and 0x40 > 0 ;
        Flag.MCE_Process       := Stat.Flags and 0x80 > 0 ;
        Flag.Superuser_Privs   := Stat.Flags and 0x100 > 0 ;
        Flag.Dumped_Core       := Stat.Flags and 0x200 > 0 ;
        Flag.Signaled          := Stat.Flags and 0x400 > 0 ;
        Flag.Memory_Allocating := Stat.Flags and 0x800 > 0 ;
    end Stat_Flags;

end Processes.Stat.PF_Flags;
