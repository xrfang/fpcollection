program demo;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, datatable;

function ln_viewer(data, default: Double): Double;
begin
  if data > 0 then
    Result := Ln(data)
  else
    Result := default;
end;

var
  t, t2 : TDataTable;
  i, j : Integer;
  r : TDataTable.Row;
  fn : string;
begin
  t := TDataTable.Create;
  {
  //commented out to test Load/Saving of tables with empty column headers
  t.Headers[0] := 't';
  t.Headers[1] := 'o';
  t.Headers[2] := 'h';
  t.Headers[3] := 'l';
  t.Headers[4] := 'c';
  }
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
  WriteLn('Viewing table data with Ln filtering...');
  t2 := TDataTable.Create(t, @ln_viewer);
  t.Free;
  WriteLn(Format('Table has %d columns and %d rows', [t2.Cols, t2.Rows]));
  Write(t2.Headers[0]);
  for i := 1 to t2.Cols do Write(',' + t2.Headers[i]);
  WriteLn;
  for i := 0 to t2.Rows - 1 do begin
    Write(t2[i].Header);
    for j := 1 to t2.Cols do Write(Format(',%0.2f', [t2[i][j]]));
    WriteLn;
  end;
  WriteLn('Data saved and loaded from ' + ExtractFileName(fn) + ' successfully.');
  t2.Free;
end.

