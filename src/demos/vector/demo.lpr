program demo;
{$mode objfpc}{$H+}
uses sysutils, vector;
type
  TIntVector = specialize TVector<Integer>;

procedure Dump(v: TIntVector);
var
  i: Integer;
begin
  WriteLn('count=', v.Count);
  for i := 0 to v.Count - 1 do Write(v[i], #9);
  WriteLn;
end;

var
  iv : TIntVector;
  i : Integer;
  r: TIntVector.DataType;
begin
  iv := TIntVector.Create(-1);
  for i := 10 to 20 do iv[i] := i + 1;
  iv.Push(123);
  Dump(iv);
  WriteLn('Trim 1 element...');
  iv.Trim(1);
  Dump(iv);
  WriteLn('Trim 10 elements...');
  iv.Trim(10);
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
  WriteLn('Sort iv reversed, eliminating NA values...');
  iv.Sort([soReversed, soEliminateNA]);
  Dump(iv);
  iv.Free;
  WriteLn('r is still accessible after iv is Destroyed, its content is unchanged:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
end.

