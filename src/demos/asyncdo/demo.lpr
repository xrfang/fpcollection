program demo;
{$mode objfpc}{$H+}
uses sysutils, asyncdo;

procedure Task(input: PtrUInt);
begin
  WriteLn('Working on task #', input);
  Sleep(1000 * (Random(5) + 1));
  WriteLn('Finished taske #', input);
end;

var
  ad: TAsyncDo;
  i, n: Integer;

begin
  Randomize;
  ad := TAsyncDo.Create(4, @Task);
  for i := 0 to 10 do begin
    n := Random(10) + 1;
    WriteLn('Submitting task #', n, ' ... res=', ad.Call(n));
  end;
  ad.Finish;
  ad.Free;
end.

