program demo3;
{$mode objfpc}{$H+}
uses types, vector;
var
  sl: TStringVector;
  al, hl: TIntegerVector;
  oo: TIntegerDynArray;
procedure dump;
var
  i: Integer;
begin
  WriteLn('name'#9'age'#9'height');
  for i := 0 to sl.Count - 1 do WriteLn(sl[i], #9, al[i], #9, hl[i]);
end;
begin
  Randomize;
  sl := TStringVector.Create('');
  al := TIntegerVector.Create(0);
  hl := TIntegerVector.Create(0);
  sl.Push('alice'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('cindy'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('emmily'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('gloria'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('bob'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('douglas'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('frank'); al.Push(Random(100)); hl.Push(Random(200));
  sl.Push('henry'); al.Push(Random(100)); hl.Push(Random(200));
  WriteLn('original:');
  dump;
  WriteLn;
  WriteLn('sort by name:');
  sl.Sort(False, @oo);
  al.ReOrder(oo);
  hl.ReOrder(oo);
  dump;
  WriteLn;
  WriteLn('sort by age:');
  al.Sort(False, @oo);
  sl.ReOrder(oo);
  hl.ReOrder(oo);
  dump;
  WriteLn;
  WriteLn('sort by height:');
  hl.Sort(False, @oo);
  sl.ReOrder(oo);
  al.ReOrder(oo);
  dump;
  sl.Free;
  al.Free;
  hl.Free;
end.

