with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Calendar;
with Ada.Calendar.Formatting;
with GNAT.OS_Lib;

procedure Alarm is
    procedure Play_Alarm;
    function Next_Alarm (Start_Time : Ada.Calendar.Time;
                         Minutes : Integer)
        return Ada.Calendar.Time;
    procedure Show_Next_Alarm (Message : String;
                               Next_Alarm : Ada.Calendar.Time);

    Alarm_Minutes : constant Integer := 50;
    Break_Minutes : constant Integer := 10;

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

    procedure Show_Next_Alarm (Message : String;
                               Next_Alarm : Ada.Calendar.Time)
    is
        use Ada.Calendar.Formatting;
    begin
        Put (Message);
        Put_Line (Local_Image (Next_Alarm));
    end Show_Next_Alarm;

    Start_Time, End_Time : Ada.Calendar.Time;
    Key : Character;
begin
    loop
        Put_Line ("Press a key to start.");
        Get_Immediate (Key);

        Start_Time := Ada.Calendar.Clock;
        End_Time := Next_Alarm (Start_Time, Alarm_Minutes);

        Put_Line ("💻 Keep working!");
        Show_Next_Alarm ("Next alarm: ", End_Time);

        delay Duration (Alarm_Minutes * 60);

        Play_Alarm;

        Put_Line ("🧘 Take a break: "
            & Break_Minutes'Image
            & " minutes.");

        End_Time := Next_Alarm (Ada.Calendar.Clock, Break_Minutes);
        Show_Next_Alarm ("Come back at ", End_Time);

        delay Duration (Break_Minutes * 60);

        Put_Line ("Break done!");
    end loop;
end Alarm;
