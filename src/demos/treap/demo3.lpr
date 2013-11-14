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
  ip := TIntPair.Create;
  for i := 1 to 20 do ip.Insert(i, 2 * i);
  for n in ip.Range(5, 5) do begin
    WriteLn(Format('key=%d, val=%d', [n^.Key, n^.Value]));
    r := ip.Rank(n^.Key);
    for m in ip.Range(3, r - 1).Reversed do
      WriteLn(#9, Format('Nested: k=%d, v=%d', [m^.Key, m^.Value]));
  end;
end.

