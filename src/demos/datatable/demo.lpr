program demo;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, datatable;

function sqr_viewer(data: Double): Double;
begin
  Result := sqr(data);
end;

var
  t : TDataTable;
  i, j : Integer;
  r : TDataTable.Row;
  fn : string;
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
    for j := 1 to t.Cols do Write(Format(',%0.0f', [t[i][j]]));
    WriteLn;
  end;
  WriteLn('Save table to disk and read back...');
  fn := ChangeFileExt(ParamStr(0), '.dat');
  t.SaveToFile(fn);
  t.LoadFromFile(fn);
  WriteLn('Viewing table data...');
  t.Viewer := @sqr_viewer;
  WriteLn(Format('Table has %d columns and %d rows', [t.Cols, t.Rows]));
  Write(t.Headers[0]);
  for i := 1 to t.Cols do Write(',' + t.Headers[i]);
  WriteLn;
  for i := 0 to t.Rows - 1 do begin
    Write('sql_viewer: ' + t[i].Header);
    for j := 1 to t.Cols do Write(Format(',%0.0f', [t[i][j]]));
    WriteLn;
    Write('RAW       : ' + t[i].Header);
    for j := 1 to t.Cols do Write(Format(',%0.0f', [t.Raw[i][j]]));
    WriteLn;
  end;
  WriteLn('Data saved and loaded from ' + ExtractFileName(fn) + ' successfully.');
  t.Free;
end.

