program demo;
{$mode objfpc}{$H+}
uses filelist;
var
  i: Integer;
begin
  with TFileList.Create(ParamStr(1)) do try
    for i := 0 to Count - 1 do WriteLn(i+1, ': ', Strings[i]);
  finally
    Free;
  end;
end.

