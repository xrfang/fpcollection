program demo4;
{$mode objfpc}{$H+}
uses vector;

var
  i: Integer;
  nl: TIntegerVector;
  _first, _last: Integer;
  order: array of Integer;
  passed: array of Integer;
begin
  Randomize;
  nl := TIntegerVector.Create(0);
  for i := 1 to 20 do nl.Push(Random(120));
  WriteLn('Original data:');
  for i := 0 to nl.Count - 1 do Write(nl[i], #9);
  WriteLn;
  nl.Sort(False, @order);
  _first := nl.Count;
  for i := 0 to nl.Count - 1 do if nl[i] >= 60 then begin
    _first := i;
    Break;
  end;
  _last := -1;
  for i := nl.Count - 1 downto 0 do if nl[i] <= 100 then begin
    _last := i;
    Break;
  end;
  passed := nl.Dump(_first, _last);
  WriteLn('Sorted:');
  for i := 0 to nl.Count - 1 do Write(nl[i], #9);
  WriteLn;
  nl.ReOrder(order, True);
  WriteLn('Restored original order:');
  for i := 0 to nl.Count - 1 do Write(nl[i], #9);
  WriteLn;
  WriteLn('Values between 60 ~ 100:');
  for i := 0 to Length(passed) - 1 do Write(passed[i], #9);
  WriteLn;
  nl.Free;
  passed := nil;
end.

