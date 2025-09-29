--  processes-stats-pf_flags.ads ---

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

--  Per Process Flags
package Processes.Stats.PF_Flags is

    type Flag_Type is record
        Virtual_CPU : Boolean;
        IDLE_Thread : Boolean;
        Exiting : Boolean;
        Postcoredump : Boolean;
        IO_Worker : Boolean;
        Workqueue_Worker : Boolean;
        Fork_No_Exec : Boolean;
        MCE_Process : Boolean;
        Superuser_Privs : Boolean;
        Dumped_Core : Boolean;
        Signaled : Boolean;
        Memory_Allocating : Boolean;
    end record;

    function Stat_Flags (Stat : Stat_Type) return Flag_Type;

end Processes.Stats.PF_Flags;
