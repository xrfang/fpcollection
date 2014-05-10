program demo;
{$mode objfpc}{$H+}
uses sysutils, vector;
procedure Dump(v: TIntegerVector);
var
  i: Integer;
begin
  WriteLn('count=', v.Count);
  for i := 0 to v.Count - 1 do Write(v[i], #9);
  WriteLn;
end;

var
  iv : TIntegerVector;
  i : Integer;
  r: array of Integer;
begin
  iv := TIntegerVector.Create(-1);
  iv.Sort;
  for i := 10 to 20 do iv[i] := i + 1;
  iv.Push(123);
  Dump(iv);
  WriteLn('Trim 1 element from head...');
  iv.First := iv.First + 1;
  Dump(iv);
  WriteLn('Trim 10 elements from tail...');
  iv.Last := iv.Last - 10;
  Dump(iv);
  r := iv.Raw;
  WriteLn('Raw content of iv:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
  iv[0] := 13;
  WriteLn('Raw content of iv after change:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
  WriteLn('Sort iv...');
  iv.Sort;
  Dump(iv);
  WriteLn('Sort iv reversed...');
  iv.Sort(True);
  Dump(iv);
  iv.Free;
  WriteLn('r is still accessible after iv is Destroyed, its content is unchanged:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
end.

