--  processes.adb ---

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

with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Directories;
with Ada.Text_IO;
use Ada.Text_IO;

package body Processes is

    function Find_Process (Name_Substring : String; Owner : String)
        return Process_Type
    is
        use Ada.Directories;

        function Has_Substring (Substring, Complete_String : String)
            return Boolean;

        function Has_Substring (Substring, Complete_String : String)
            return Boolean
        is
            use Ada.Strings.Fixed;
        begin
            return Index (Complete_String, Substring,
                          Complete_String'First) > 0;
        end Has_Substring;

        Proc_Search : Search_Type;
        Found : Boolean := False;
        Stat : Process_Type;
        Directory_Entry : Directory_Entry_Type;
    begin
        Start_Search (Proc_Search, Proc_Path, "");

        while not Found and then More_Entries (Proc_Search) loop
            Get_Next_Entry (Proc_Search, Directory_Entry);

            if Kind (Directory_Entry) = Directory
                and then Simple_Name (Directory_Entry) /= "."
                and then Simple_Name (Directory_Entry) /= ".."
                and then Exists (Full_Name (Directory_Entry) & "/comm")
                and then Kind (Full_Name (Directory_Entry) & "/comm") =
                         Ordinary_File
            then
                begin
                    Stat.PID := PID_Type'Value (Simple_Name (Directory_Entry));
                    Stat.Command := Read_Comm (Full_Name (Directory_Entry)
                                                & "/comm");
                    Found := Has_Substring (Name_Substring,
                                            String (Stat.Command));
                exception
                    when others =>
                        null;
                end;
            end if;
        end loop;

        End_Search (Proc_Search);

        if not Found then
            Stat := Invalid_Process;
        end if;

        return Stat;
    end Find_Process;

    function Find_Process (PID : PID_Type) return Process_Type
    is
        Process : Process_Type;
        Path : constant String := Process_Path (PID);
    begin
        if not Ada.Directories.Exists (Path) then
            return Invalid_Process;
        end if;

        Process.PID := PID;
        Process.Command := Read_Comm (PID);

        return Process;
    end Find_Process;

    function List_Processes return Process_Vector is
        use Ada.Directories;
        use Process_Vector_Pack;

        Processes : Process_Vector;
        Proc_Search : Search_Type;
        Stat : Process_Type;
        Directory_Entry : Directory_Entry_Type;
    begin
        Start_Search (Proc_Search, Proc_Path, "*");

        while More_Entries (Proc_Search) loop
            Get_Next_Entry (Proc_Search, Directory_Entry);

            if Kind (Directory_Entry) = Directory
                and then Simple_Name (Directory_Entry) /= "."
                and then Simple_Name (Directory_Entry) /= ".."
                and then Exists (Full_Name (Directory_Entry) & "/comm")
                and then Kind (Full_Name (Directory_Entry) & "/comm") =
                         Ordinary_File
            then
                begin
                    Stat.PID := PID_Type'Value (Simple_Name (Directory_Entry));
                    Stat.Command := Read_Comm (Full_Name (Directory_Entry)
                                                & "/comm");
                    Append (Processes, Stat);
                exception
                    when others =>
                        null;
                end;
            end if;
        end loop;

        End_Search (Proc_Search);

        return Processes;
    end List_Processes;

    --  Moved to Processes.Stats.
    --
    --  function Parse_Stat_String (S : String) return Process_Type
    --  is
    --      use Ada.Strings.Fixed;
    --      Stat : Process_Type;
    --      I, J : Positive;
    --  begin
    --      I := S'First;
    --      J := Index (S, " ", I) - 1;
    --      Stat.PID := PID_Type'Value (S (I .. J));

    --      I := J + 3; --  Ignore the " ("
    --      J := Index (S, ") ", I) - 1;
    --      Stat.Command (1 .. J - I + 1) := S (I .. J);

    --      I := J + 3;
    --      J := I;
    --      Stat.State := State_String_To_Type (S (I .. J));

    --      I := J + 2;
    --      J := Index (S, " ", I) - 1;
    --      Stat.Parent_PID := PID_Type'Value (S (I .. J));

    --      return Stat;
    --  end Parse_Stat_String;

    function Process_Path (PID : PID_Type) return String
        is (Proc_Path & "/"
            & Ada.Strings.Fixed.Trim (PID'Image, Ada.Strings.Both));

    --  Moved to Processes.Stats.
    --
    --  function Process_Stat (Stat_File_Path : String) return Process_Type
    --  is
    --      Process_File : File_Type;
    --      Process_Stat : Process_Type;
    --  begin
    --      Open (Process_File, In_File, Stat_File_Path);
    --      declare
    --          Line : constant String := Get_Line (Process_File);
    --      begin
    --          Process_Stat := Parse_Stat_String (Line);
    --      end;
    --      Close (Process_File);

    --      return Process_Stat;
    --  end Process_Stat;

    --  Moved to Processes.Stats.
    --
    --  function Process_Stat (PID : PID_Type) return Process_Type
    --      is (Process_Stat (Proc_Path
    --            & Ada.Strings.Fixed.Trim (PID'Image, Ada.Strings.Both)
    --            & "/stat"));

    function Read_Comm (Path : String) return Comm_String is
        Comm_File : File_Type;
        Str : Comm_String;
    begin
        Open (Comm_File, In_File, Path);
        declare
            Line : constant String := Get_Line (Comm_File);
        begin
            Str := Comm_String (Line (1 .. 16));
        end;
        Close (Comm_File);

        return Str;
    end Read_Comm;

end Processes;
