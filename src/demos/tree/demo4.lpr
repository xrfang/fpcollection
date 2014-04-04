program demo4;
{$mode objfpc}{$H+}
uses types, Classes, sysutils, tree;
type
  TIntTree = specialize TTree<Integer>;
procedure Dump(t: TIntTree);
begin
  while t <> nil do begin
    WriteLn(Format('%s%d', [StringOfChar(' ', t.Level * 2), t.Data]));
    t := TIntTree(t.Next);
  end;
end;

var
  it1, it2, it3: TIntTree;
begin
  it1 := TIntTree.Create(0, nil);
  TIntTree.Create(11, TIntTree.Create(1, it1));
  TIntTree.Create(22, TIntTree.Create(21, TIntTree.Create(2, it1)).Parent);
  TIntTree.Create(33, TIntTree.Create(32, TIntTree.Create(31, TIntTree.Create(3,
    it1)).Parent).Parent);
  Dump(it1);
  it2 := it1.Clone;
  Dump(it2);
  it3 := TIntTree.Create(0, nil);
  TIntTree.Create(11, TIntTree.Create(1, it3));
  TIntTree.Create(22, TIntTree.Create(21, TIntTree.Create(2, it3)).Parent);
  Dump(it3);
  WriteLn('Comparing it1 and it2: ', it1.Compare(it2));
  WriteLn('Comparing it2 and it1: ', it2.Compare(it1));
  WriteLn('Comparing it1 and it3: ', it1.Compare(it3));
  WriteLn('Comparing it3 and it1: ', it3.Compare(it1));
end.

