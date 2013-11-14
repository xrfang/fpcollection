program demo3;
{$mode objfpc}{$H+}
uses sysutils, treap;
type
  TIntPair = specialize TTreap<Integer, Integer>;
var
  ip: TIntPair;
  n, m: TIntPair.PNode;
  i, r: Integer;
begin
  WriteLn('See if nested enumeration works...');
  ip := TIntPair.Create;
  for i := 1 to 20 do ip.Insert(i, 2 * i);
  WriteLn('Ensure no items are enumerated when StartIdx is out-of-range:');
  for n in ip.Range(5, 16) do begin
    WriteLn(Format('key=%d, val=%d', [n^.Key, n^.Value]));
    r := ip.Rank(n^.Key);
    for m in ip.Range(3, r + 1) do
      WriteLn(#9, Format('Nested: k=%d, v=%d', [m^.Key, m^.Value]));
  end;
  WriteLn('Reversed nested enumeration:');
  for n in ip.Range(5, 7) do begin
    Write(Format('key=%d, val=%d', [n^.Key, n^.Value]));
    r := ip.Rank(n^.Key);
    Write(#9'Nested: ');
    for m in ip.Reversed.Range(3, r - 1) do Write(m^.Key, '  ');
    WriteLn;
  end;
  WriteLn('See if Reversed() and Range() are interchangeable...');
  WriteLn('No StartIdx:');
  Write('Reversed before Range: ');
  for n in ip.Reversed.Range(5) do Write(n^.Key, '  ');
  WriteLn;
  Write('Range before Reversed: ');
  for n in ip.Range(5).Reversed do Write(n^.Key, '  ');
  WriteLn;
  WriteLn('StartIdx < Count');
  Write('Reversed before Range: ');
  for n in ip.Reversed.Range(5, 3) do Write(n^.Key, '  ');
  WriteLn;
  Write('Range before Reversed: ');
  for n in ip.Range(5, 3).Reversed do Write(n^.Key, '  ');
  WriteLn;
  WriteLn('StartIdx > Count');
  Write('Reversed before Range: ');
  for n in ip.Reversed.Range(5, 7) do Write(n^.Key, '  ');
  WriteLn;
  Write('Range before Reversed: ');
  for n in ip.Range(5, 7).Reversed do Write(n^.Key, '  ');
  WriteLn;

end.

