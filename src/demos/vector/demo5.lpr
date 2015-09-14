program demo5;
{$mode objfpc}{$H+}
uses vector;

var
  i, v: Integer;
  nl: TIntegerVector;
begin
  Randomize;
  nl := TIntegerVector.Create(0);
  for i := 1 to 10 do nl.Push(Random(120));
  v := nl[5];
  for i := 1 to 5 do nl.Push(v);
  WriteLn('Original data:');
  for i := 0 to nl.Count - 1 do Write(i, #9); WriteLn;
  for i := 0 to nl.Count - 1 do Write(nl[i], #9); WriteLn;
  WriteLn('Position of 150 (forward): ', nl.Seek(150));
  WriteLn('Position of 150 (backward): ', nl.Seek(150, True));
  WriteLn('Position of ', v, ' (forward): ', nl.Seek(v));
  WriteLn('Position of ', v, ' (backward): ', nl.Seek(v, True));
  nl.Sort;
  WriteLn('Sorted (ascending):');
  for i := 0 to nl.Count - 1 do Write(i, #9); WriteLn;
  for i := 0 to nl.Count - 1 do Write(nl[i], #9); WriteLn;
  WriteLn('Position of ', v, ' (forward): ', nl.Seek(v));
  WriteLn('Position of ', v, ' (backward): ', nl.Seek(v, True));
  nl.Sort(True);
  WriteLn('Sorted (descending):');
  for i := 0 to nl.Count - 1 do Write(i, #9); WriteLn;
  for i := 0 to nl.Count - 1 do Write(nl[i], #9); WriteLn;
  WriteLn('Position of ', v, ' (forward): ', nl.Seek(v));
  WriteLn('Position of ', v, ' (backward): ', nl.Seek(v, True));
end.
