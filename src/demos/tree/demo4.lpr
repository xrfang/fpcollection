program demo4;
{$mode objfpc}{$H+}
uses types, Classes, sysutils, tree;
type
  TIntTree = class(specialize TTree<Integer>)
  protected
    function DoCompare(Target: TSelfType; P: PtrUInt): Integer; override;
  end;
procedure Dump(t: TIntTree);
begin
  while t <> nil do begin
    WriteLn(Format('%s%d', [StringOfChar(' ', t.Level * 2), t.Data]));
    t := TIntTree(t.Next);
  end;
end;

var
  it1, it2, it3: TIntTree;

function TIntTree.DoCompare(Target: TSelfType; P: PtrUInt): Integer;
begin
  if Data = Target.Data then Exit(0);
  if Data < Target.Data then Exit(-1);
  Result := 1;
end;

function RandTree: TIntTree;
begin
  Result := TIntTree.Create(Random(1000), nil);
  TIntTree.Create(Random(1000), TIntTree.Create(Random(1000), Result));
  TIntTree.Create(Random(1000), TIntTree.Create(Random(1000),
    TIntTree.Create(Random(1000), Result)).Parent);
end;

begin
  it1 := TIntTree.Create(0, nil);
  TIntTree.Create(11, TIntTree.Create(1, it1));
  TIntTree.Create(22, TIntTree.Create(21, TIntTree.Create(2, it1)).Parent);
  TIntTree.Create(33, TIntTree.Create(32, TIntTree.Create(31, TIntTree.Create(3,
    it1)).Parent).Parent);
  Dump(it1);
  it2 := TIntTree(it1.Clone);
  Dump(it2);
  it3 := TIntTree.Create(0, nil);
  TIntTree.Create(11, TIntTree.Create(1, it3));
  TIntTree.Create(22, TIntTree.Create(21, TIntTree.Create(2, it3)).Parent);
  Dump(it3);
  WriteLn('Comparing it1 and it2: ', it1.Compare(it2));
  WriteLn('Comparing it2 and it1: ', it2.Compare(it1));
  WriteLn('Comparing it1 and it3: ', it1.Compare(it3));
  WriteLn('Comparing it3 and it1: ', it3.Compare(it1));
  it1.Free; it2.Free; it3.Free;
  Randomize;
  it1 := RandTree;
  Dump(it1);
  it2 := RandTree;
  Dump(it2);
  WriteLn('Comparing it1 and it2: ', it1.Compare(it2));
  WriteLn('Comparing it2 and it1: ', it2.Compare(it1));
  it1.Free; it2.Free;
end.

