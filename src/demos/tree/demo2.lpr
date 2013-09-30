program demo2;
{$mode objfpc}{$H+}
uses sysutils, tree;
type
  TIntTree = class(specialize TTree<Integer>)
  private
    FSalt: Double;
  protected
    procedure DoClone(Source: TSelfType; var Target: TSelfType); override;
  public
    constructor Create(AData: Integer; AParent: TIntTree); reintroduce;
  end;
  TOddTree = class(TIntTree)
  protected
    procedure DoClone(Source: TSelfType; var Target: TSelfType); override;
  end;

procedure PrintTree(tr: TIntTree);
var
  n: TIntTree;
begin
  n := tr;
  while n <> nil do begin
    WriteLn(Format('%s%d'#9'%0.2f', [StringOfChar(' ', n.Level * 2), n.Data, n.FSalt]));
    n := TIntTree(n.Next);
  end;
end;

procedure TOddTree.DoClone(Source: TSelfType; var Target: TSelfType);
begin
  inherited DoClone(Source, Target);
  if Target.Data mod 2 = 0 then FreeAndNil(Target);
end;

procedure TIntTree.DoClone(Source: TSelfType; var Target: TSelfType);
var
  _s, _t: TIntTree;
begin
  _s := Source as TIntTree;
  _t := Target as TIntTree;
  _t.Data *= 3;
  _t.FSalt := _s.FSalt;
end;

constructor TIntTree.Create(AData: Integer; AParent: TIntTree);
begin
  inherited Create(AData, AParent);
  FSalt := Random;
end;

var
  it1, it2: TIntTree;
  it3, it4 : TOddTree;
  i: Integer;
begin
  Randomize;
  it1 := TIntTree(TIntTree.Create(2, TIntTree.Create(1, nil)).Root);
  it2 := it1.Clone as TIntTree;
  WriteLn('ClassName of original instance: ', it1.ClassName);
  PrintTree(it1);
  WriteLn('ClassName of cloned instance: ', it2.ClassName);
  PrintTree(it2);
  WriteLn('Prepare a new OddTree with 10 children');
  it3 := TOddTree.Create(1, nil);
  for i := 2 to 11 do TOddTree.Create(i, it3);
  PrintTree(it3);
  WriteLn('Clone the OddTree (removing even nodes)');
  it4 := it3.Clone as TOddTree;
  PrintTree(it4);
end.


