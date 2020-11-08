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

with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Widgets.Selectors is

    type Selector_Type is tagged private;

    package Data_Vectors is new Ada.Containers.Vectors
      (Element_Type => Unbounded_String,
       Index_Type => Positive);
    package Data_Vectors_Sorting is new Data_Vectors.Generic_Sorting
      ("<" => "<");

    subtype Data_Vector is Data_Vectors.Vector;

    procedure Execute (Selector : in out Selector_Type);

    procedure Set_Current_String (Selector : in out Selector_Type;
                                  Current_String : Unbounded_String);
    function Get_Current_String (Selector : Selector_Type)
                                return Unbounded_String;
    function Get_Current_String (Selector : Selector_Type)
                                return String;

    procedure Set_Data (Selector : in out Selector_Type;
                        Data : Data_Vector);
    function Get_Data (Selector : Selector_Type)
                      return Data_Vector;

    function Filter_Data (Selector : Selector_Type;
                          Substring : Unbounded_String)
                         return Data_Vector;
    function Filter_Data (Selector : Selector_Type; Substring : String)
                         return Data_Vector;

    procedure Add (Selector : in out Selector_Type;
                   Data : String);

private

    type Selector_Type is tagged record
        Data : Data_Vector;
        Current_String : Unbounded_String;
    end record;

    procedure Put_Data (Selector : Selector_Type);

end Widgets.Selectors;