program demo;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, datatable;
var
  t : TDataTable;
  i, j : Integer;
  r : TDataTable.Row;
begin
  t := TDataTable.Create;
  t.Headers[0] := 't';
  t.Headers[1] := 'o';
  t.Headers[2] := 'h';
  t.Headers[3] := 'l';
  t.Headers[4] := 'c';
  for i := 0 to 5 do begin
    r := t.Append;
    r.Header := '2013-02-22';
    for j := 1 to 4 do r[j] := i * j;
  end;
  t.Append[1] := 124;
  t.Delete(1);
  WriteLn(Format('Table has %d columns and %d rows', [t.Cols, t.Rows]));
  Write(t.Headers[0]);
  for i := 1 to t.Cols do Write(',' + t.Headers[i]);
  WriteLn;
  for i := 0 to t.Rows - 1 do begin
    Write(t[i].Header);
    for j := 1 to t.Cols do Write(Format(',%0.2f', [t[i][j]]));
    WriteLn;
  end;
  t.Free;
end.

