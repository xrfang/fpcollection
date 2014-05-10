program demo2;
{$mode objfpc}{$H+}{$ASSERTIONS on}
uses sysutils, vector;

var
  i, c, v, len: Integer;
  iv: TIntegerVector;
  rn: string;
  p: PChar;
begin
  Randomize;
  iv := TIntegerVector.Create(-1);
  if ParamCount <> 1 then begin
    p := strrscan(PChar(ParamStr(0)), DirectorySeparator);
    if p <> nil then rn := p + 1 else rn := ParamStr(0);
    WriteLn(stderr, 'USAGE: ', rn, ' <vector-length>');
    Exit;
  end;
  len := StrToInt(ParamStr(1));
  WriteLn(stderr, 'len=', len);
  for c := 0 to 20 do begin
    iv.Clear;
    for i := 0 to len - 1 do iv.Push(round(1000 * Random));
    iv.Sort;
    Assert(iv.Count = len);
    v := -100000;
    for i := 0 to iv.Count - 1 do begin
      Assert(iv[i] >= v);
      v := iv[i];
      Write(iv[i], '  ');
    end;
    WriteLn;
  end;
  iv.Free;
end.

