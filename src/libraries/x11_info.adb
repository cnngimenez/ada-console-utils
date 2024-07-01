--  x11_info.adb ---

--  Copyright 2024 cnngimenez
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

with System;
use System;

package body X11_Info is

    type Xss_Info is record
        Window : unsigned_long;
        State : int;
        Kind : int;
        Til_Or_Since : unsigned_long;
        Idle : unsigned_long;
        Event_Mask : unsigned_long;
    end record
        with Convention => C;

    procedure Xscreensaverqueryinfo (Display : System.Address;
                                     Drawable : System.Address;
                                     Info : out Xss_Info);
    pragma Import (C, Xscreensaverqueryinfo, "XScreenSaverQueryInfo");

    --  function Xscreensaverallocinfo return System.Address;
    --  pragma Import (C, xscreensaverallocinfo, "XScreenSaverAllocInfo");

    function Xopendisplay (Name : System.Address) return System.Address;
    pragma Import (C, Xopendisplay, "XOpenDisplay");

    procedure Xclosedisplay (Dpy : System.Address);
    pragma Import (C, Xclosedisplay, "XCloseDisplay");

    --  procedure Xfree (Data: System.Address);
    --  pragma Import (C, Xfree, "XFree");

    function Xdefaultrootwindow (Display : System.Address)
                                return System.Address;
    pragma Import (C, Xdefaultrootwindow, "XDefaultRootWindow");

    function Screensaver_Default_Info return Screensaver_Info_Type is
        Display : System.Address;
        Info : Xss_Info;
        Ret : Screensaver_Info_Type;
    begin
        Display := Xopendisplay (Null_Address);

        if Display = Null_Address then
            return Invalid_Screensaver_Info;
        end if;

        --  Info := Xscreensaverallocinfo;

        Xscreensaverqueryinfo (Display, Xdefaultrootwindow (Display), Info);

        --  Xfree (Info);
        Xclosedisplay (Display);

        Ret.Invalid := False;
        Ret.State := Integer (Info.State);
        Ret.Kind := Integer (Info.Kind);
        Ret.Til_Or_Since := Info.Til_Or_Since;
        Ret.Idle := Info.Idle;

        return Ret;
    end Screensaver_Default_Info;

end X11_Info;
