program demo;
{$mode objfpc}{$H+}
uses
  Classes, sysutils, dsheet;

var
  t : TDataSheet;
  i, j : Integer;
  r : TDataSheet.Row;
  v: Real;
  fn : string;
begin
  t := TDataSheet.Create;
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
  WriteLn('Save table to disk and read back...');
  fn := ChangeFileExt(ParamStr(0), '.dat');
  t.Save(fn);
  t.Load(fn);
  WriteLn('Viewing loaded table data...');
  WriteLn(Format('Table has %d columns and %d rows', [t.Cols, t.Rows]));
  Write(t.Headers[0]);
  for i := 1 to t.Cols do Write(',' + t.Headers[i]);
  WriteLn;
  for i := 0 to t.Rows - 1 do begin
    Write(t[i].Header);
    for j := 1 to t.Cols do begin
      v := t[i][j];
      Write(Format(',%0.2f', [v]));
    end;
    WriteLn;
  end;
  WriteLn('Data saved and loaded from ' + ExtractFileName(fn) + ' successfully.');
  t.Free;
end.
