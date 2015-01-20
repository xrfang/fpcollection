program demo;
{$mode objfpc}{$H+}
uses sysutils, asyncdo;

type
  TAsyncDemo = class(TAsyncDo)
  protected
    procedure Task(input: PtrUInt); override;
  end;

procedure TAsyncDemo.Task(input: PtrUInt);
begin
  WriteLn('Working on task #', input);
  Sleep(1000 * (Random(5) + 1));
  WriteLn('Finished taske #', input);
end;

var
  ad: TAsyncDemo;
  i, n: Integer;
begin
  Randomize;
  ad := TAsyncDemo.Create(4);
  for i := 0 to 10 do begin
    n := Random(10) + 1;
    WriteLn('Submitting task #', n, ' ... res=', ad.Call(n));
  end;
  ad.Finish;
end.

