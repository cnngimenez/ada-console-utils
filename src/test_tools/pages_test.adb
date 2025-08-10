with Ada.Text_IO;
use Ada.Text_IO;
with Apagerlib.Pages;
use Apagerlib.Pages;

procedure Pages_Test is
    procedure Print_Page;

    Page : Page_Type;

    procedure Print_Page is
    begin
        Put_Line ("Data:");
        for I in 1 .. Page.Length loop
            Put (Page.Data (Page_Index (I)));
        end loop;
        Put ("EOP");
        Put_Line ("--  End of page (EOP)  --");
        Put_Line ("Page length: " & Page.Length'Image);
        Put_Line ("Page line start: " & Page.Line_Start'Image);
        Put_Line ("Page line end: " & Page.Line_End'Image);
    end Print_Page;

begin
    Put_Line ("Page_Limit (constant): "
        & Apagerlib.Pages.Page_Limit'Image);
    while not End_Of_File loop
        Put_Line ("Reading page...");
        Get_Page (Page, 1);
        Put_Line ("Page red.");

        Print_Page;
    end loop;
end Pages_Test;
