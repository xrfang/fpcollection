program demo;

{$mode objfpc}{$H+}

uses
  Classes, sysutils, datatable;
type
  TDoubles = array of Double;
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
  ds : TDoubles;
  r : TDataTable.Row;
  v: Real;
  fn : string;
begin
  t := TDataTable.Create;
  //comment out to test Load/Saving of tables with empty column headers
  t.Headers[0] := 't';
  t.Headers[1] := 'o';
  t.Headers[2] := 'h';
  //comment out to test partial header situation
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
    for j := 1 to t.Cols do begin
      v := t[i][j];
      Write(Format(',%0.0f', [v]));
    end;
    WriteLn;
  end;
  WriteLn('Fetch data via column header instead of column index...');
  Write(t.Headers[0]);
  for i := 1 to t.Cols do Write(',' + t.Headers[i]);
  WriteLn;
  for i := 0 to t.Rows - 1 do begin
    WriteLn(t[i]['t'],',',t[i]['o'],',',t[i]['h'],',',t[i]['l'],',',t[i]['c']);
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
    for j := 1 to t2.Cols do begin
      v := t2[i][j];
      Write(Format(',%0.2f', [v]));
    end;
    WriteLn;
  end;
  WriteLn('Data saved and loaded from ' + ExtractFileName(fn) + ' successfully.');
  WriteLn('Testing fetching with Raw property...');
  for i := 0 to t2.Rows - 1 do begin
    Write(t2[i].Header);
    ds := TDoubles(t2[i].Raw);
    for j := 0 to Length(ds) - 1 do Write(Format(',%0.2f', [ds[j]]));
    WriteLn;
  end;
  t2.Free;
end.

