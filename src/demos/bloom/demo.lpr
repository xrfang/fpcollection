program demo;
{$mode objfpc}{$H+}
uses sysutils, bloom;
var
  bf: TBloomFilter;
  p: PChar;
begin
  p := 'Hello';
  bf := TBloomFilter.Create(30);
  WriteLn(BoolToStr(bf.Contains(p, strlen(p)), True));
  bf.Add(p, strlen(p));
  WriteLn(BoolToStr(bf.Contains(p, strlen(p)), True));
  bf.Free;
end.

