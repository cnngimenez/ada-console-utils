with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.OS_Lib;
with Console.CSI_Codes;
with Processes;

procedure Alarm is
    procedure Play_Alarm;
    function Next_Alarm (Start_Time : Ada.Calendar.Time;
                         Minutes : Integer)
        return Ada.Calendar.Time;
    procedure Show_Next_Alarm (Message : String;
                               Next_Alarm : Ada.Calendar.Time);
    procedure Wait_Process (Waiting_Minutes : Natural);
    function Screenlocker_Detected return Boolean;

    Alarm_Minutes : constant Natural := 50;
    Break_Minutes : constant Natural := 10;

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

    procedure Play_Alarm is
        use GNAT.OS_Lib;

        Success : Boolean;
        Args : constant Argument_List_Access :=
            Argument_String_To_List (
                "--no-terminal " &
                "/usr/share/sounds/freedesktop/stereo/complete.oga"
                );
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

    procedure Wait_Process (Waiting_Minutes : Natural) is
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

            if not Screenlocker_Detected then
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

        Wait_Process (Alarm_Minutes);
        --  delay Duration (Alarm_Minutes * 60);

        Play_Alarm;

        Put_Line ("🧘 Take a break: "
            & Break_Minutes'Image
            & " minutes.");

        End_Time := Next_Alarm (Ada.Calendar.Clock, Break_Minutes);
        Show_Next_Alarm ("Come back at ", End_Time);

        Wait_Process (Break_Minutes);
        --  delay Duration (Break_Minutes * 60);

        Put_Line ("Break done!");
    end loop;
end Alarm;
