program demo;
{$mode objfpc}{$H+}
uses filelist;
var
  fl: TFileList;
  i: Integer;
begin
  fl := TFileList.Create;
  fl.Root := ParamStr(1);
  fl.Recursive := True;
  fl.Update;
  for i := 0 to fl.Count - 1 do WriteLn(i+1, ': ', fl[i]);
end.

