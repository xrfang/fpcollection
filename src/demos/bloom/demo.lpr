program demo;
{$mode objfpc}{$H+}
uses sysutils, bloom;
const
  ITEM_COUNT = 5000000;
var
  bf: TBloomFilter;
  p: PChar;
  b: Boolean;
  i, fpc: PtrUInt;
  t: TDateTime;
begin
  p := 'Hello';
  bf := TBloomFilter.Create(26);
  WriteLn('Size=', bf.Size);
  b := bf.Add(p, strlen(p));
  WriteLn(BoolToStr(b, True));
  WriteLn(BoolToStr(bf.Contains(p, strlen(p)), True));
  b := bf.Add(p, strlen(p));
  WriteLn(BoolToStr(b, True));
  bf.Clear;
  fpc := 0;
  t := Now;
  for i := 0 to ITEM_COUNT - 1 do begin
    if bf.Add(@i, SizeOf(PtrUInt)) then Inc(fpc);
  end;
  t := Now - t;
  WriteLn('Effective Items: ', bf.Count);
  WriteLn('Speed (Item/Sec): ', (ITEM_COUNT/t/86400):0:0);
  WriteLn('False-Positive(%):');
  WriteLn('  Theoretical: ', (bf.FalsePositive * 100):0:3);
  WriteLn('  Actual: ', (fpc / ITEM_COUNT * 100):0:3);
  bf.Free;
end.
