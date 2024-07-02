with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.OS_Lib;
with Interfaces.C;
use Interfaces.C;
with Console.CSI_Codes;
with Processes;
with X11_Info;

procedure Alarm is
    procedure Play_Alarm (Sound_File : String);
    --  Play the alarm sound.

    function Next_Alarm (Start_Time : Ada.Calendar.Time;
                         Minutes : Integer)
        return Ada.Calendar.Time;
    --  Return the next date-time when the alarm should start.

    procedure Show_Next_Alarm (Message : String;
                               Next_Alarm : Ada.Calendar.Time);
    --  Print the alarm time.

    procedure Wait_Process (Waiting_Minutes : Natural;
                            Stop_When_Idle : Boolean := True);
    --  Waiting Subprogram.
    --
    --  Wait the given minutes.

    function Screenlocker_Detected return Boolean;
    --  Is the screen saver Running?

    function Is_Idle_More_Than (Seconds : Natural) return Boolean;
    --  Is the computer idle for more than the given seconds

    Alarm_Minutes : constant Natural := 50;
    --  How many work time minutes do you have.
    Break_Minutes : constant Natural := 10;
    --  Minutes of break time!
    IDLE_Tolerance : constant Natural := 60;
    --  If the computer is IDLE from IDLE_Tolerance seconds, then stop The
    --  countdouwn.

    Alarm_Sound_Path : constant String
        := "/usr/share/sounds/freedesktop/stereo/complete.oga";
    Break_Done_Sound_Path : constant String
        := "/usr/share/sounds/freedesktop/stereo/message.Oga";

    function Is_Idle_More_Than (Seconds : Natural) return Boolean is
    begin
        return X11_Info.Idle_Time_Default >= unsigned_long (Seconds * 1000);
    end Is_Idle_More_Than;

    function Next_Alarm (Start_Time : Ada.Calendar.Time;
                         Minutes : Integer)
        return Ada.Calendar.Time
    is
        use Ada.Calendar;

        Next_Alarm_Duration : constant Duration :=
            Duration (Minutes * 60);
    begin
        return Start_Time + Next_Alarm_Duration;
    end Next_Alarm;

    procedure Play_Alarm (Sound_File : String) is
        use GNAT.OS_Lib;

        Success : Boolean;
        Args : constant Argument_List_Access :=
            Argument_String_To_List ("--no-terminal " & Sound_File);
    begin
        Put (Character'Val (7));
        Put_Line ("🔔 ¡Alarm!");
        --  Play Alarm
        Spawn ("/usr/bin/mpv", Args.all, Success);
    end Play_Alarm;

    function Screenlocker_Detected return Boolean is
        use Processes;

        Stat : Process_Type;
    begin
        Stat := Find_Process ("screenlock");
        return Stat /= Invalid_Process;
    end Screenlocker_Detected;

    procedure Show_Next_Alarm (Message : String;
                               Next_Alarm : Ada.Calendar.Time)
    is
        use Ada.Calendar.Formatting;
    begin
        Put (Message);
        Put_Line (Local_Image (Next_Alarm));
    end Show_Next_Alarm;

    procedure Wait_Process (Waiting_Minutes : Natural;
                            Stop_When_Idle : Boolean := True) is
        type Seconds_Type is range 0 .. 60;
        Rest_Minutes, Minutes_Elapsed : Natural := 0;
        Rest_Seconds : Seconds_Type := 60;
    begin
        Put_Line ("Time to next step: ");
        while Minutes_Elapsed <= Waiting_Minutes  loop
            Rest_Minutes := Waiting_Minutes - Minutes_Elapsed;
            Put (Rest_Minutes'Image & " minutes "
                & Rest_Seconds'Image & " seconds...");

            delay 1.0;

            Console.CSI_Codes.Erase_Line (Console.CSI_Codes.Entire_Line);
            Console.CSI_Codes.Cursor_Previous_Line;
            Console.CSI_Codes.Cursor_Next_Line;

            if not (Stop_When_Idle
                    and then (Screenlocker_Detected
                              or else Is_Idle_More_Than (IDLE_Tolerance)))
            then
                --  if not Screenlocker_Detected then
                Rest_Seconds := Rest_Seconds - 1;

                if Rest_Seconds = 0 then
                    Minutes_Elapsed := Minutes_Elapsed + 1;
                    Rest_Seconds := 60;
                end if;
            end if;
        end loop;
    end Wait_Process;

    Start_Time, End_Time : Ada.Calendar.Time;
    Key : Character;
begin
    loop
        Put_Line ("Press a key to start the alarm.");
        Get_Immediate (Key);

        Start_Time := Ada.Calendar.Clock;
        End_Time := Next_Alarm (Start_Time, Alarm_Minutes);

        Put_Line ("💻 Keep working!");
        Show_Next_Alarm ("Next alarm: ", End_Time);

        Wait_Process (Alarm_Minutes, True);
        --  delay Duration (Alarm_Minutes * 60);

        Play_Alarm (Alarm_Sound_Path);

        Put_Line ("🧘 Take a break: "
            & Break_Minutes'Image
            & " minutes.");

        End_Time := Next_Alarm (Ada.Calendar.Clock, Break_Minutes);
        Show_Next_Alarm ("Come back at ", End_Time);

        Wait_Process (Break_Minutes, False);
        --  delay Duration (Break_Minutes * 60);

        Play_Alarm (Break_Done_Sound_Path);

        Put_Line ("Break done!");
    end loop;
end Alarm;
