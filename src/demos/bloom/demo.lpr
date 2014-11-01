program demo;
{$mode objfpc}{$H+}
uses bloom;
var
  bf: TBloomFilter;
begin
  bf := TBloomFilter.Create(32);
  bf.Free;
end.

