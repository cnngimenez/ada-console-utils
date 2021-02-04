--  widgets-selectors.ads ---

--  Copyright 2020 cnngimenez
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

with Ada.Strings.Wide_Wide_Unbounded;
use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Vectors;

generic
    --  Procedure executed when the user press enter on a selection.
    with procedure On_Selected_Callback (Current_String : Wide_Wide_String);
package Widgets.Selectors is

    type Selector_Type is tagged private;

    package Data_Vectors is new Ada.Containers.Vectors
      (Element_Type => Unbounded_Wide_Wide_String,
       Index_Type => Positive);
    package Data_Vectors_Sorting is new Data_Vectors.Generic_Sorting
      ("<" => "<");

    subtype Data_Vector is Data_Vectors.Vector;

    procedure Initialize (Selector : in out Selector_Type);

    procedure Draw (Selector : in out Selector_Type);

    procedure Execute (Selector : in out Selector_Type);

    procedure Set_Current_String (Selector : in out Selector_Type;
                                  Current_String : Unbounded_Wide_Wide_String);
    function Get_Current_String (Selector : Selector_Type)
                                return Unbounded_Wide_Wide_String;
    function Get_Current_String (Selector : Selector_Type)
                                return Wide_Wide_String;

    procedure Set_Data (Selector : in out Selector_Type;
                        Data : Data_Vector);
    function Get_Data (Selector : Selector_Type)
                      return Data_Vector;

    function Filter_Data (Selector : Selector_Type;
                          Substring : Unbounded_Wide_Wide_String)
                         return Data_Vector;
    function Filter_Data (Selector : Selector_Type;
                          Substring : Wide_Wide_String)
                         return Data_Vector;
    function Current_Filter_Data (Selector : Selector_Type)
                                 return Data_Vector;

    procedure Add (Selector : in out Selector_Type;
                   Data : Wide_Wide_String);

    procedure Next_Selection (Selector : in out Selector_Type);
    procedure Previous_Selection (Selector : in out Selector_Type);

    procedure Key_Event (Selector : in out Selector_Type; Key : Character);

private
    type Last_Event_Array_Type is array (1 .. 2) of Character;

    type Selector_Type is tagged record
        Data : Data_Vector;
        Current_String : Unbounded_Wide_Wide_String;
        Current_Selection : Positive;

        Last_Key_Event : Last_Event_Array_Type;
    end record;

    procedure Ask_If_New (Selector : in out Selector_Type);
    --  Delete one character at the end of the current written string
    --  (if possible).
    procedure Delete_Character (Selector : in out Selector_Type);
    procedure Put_Data (Selector : Selector_Type);

end Widgets.Selectors;
