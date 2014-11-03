program demo;
{$mode objfpc}{$H+}
uses sysutils, bloom;
var
  bf: TBloomFilter;
  p: PChar;
  b: Boolean;
begin
  p := 'Hello';
  bf := TBloomFilter.Create(32);
  b := bf.Add(p, strlen(p));
  WriteLn(BoolToStr(b, True));
  WriteLn(BoolToStr(bf.Contains(p, strlen(p)), True));
  b := bf.Add(p, strlen(p));
  WriteLn(BoolToStr(b, True));
  bf.Free;
end.

