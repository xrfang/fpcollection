program demo2;
{$mode objfpc}{$H+}
uses tree;
type
  TIntTree = class(specialize TTree<Integer>)
  public
    function Clone: TIntTree;
  end;

function TIntTree.Clone: TIntTree;
begin
  Result := TIntTree.Create(Data, FParent);
  DoClone(Result);
end;

var
  it1, it2 : TIntTree;
begin
  it1 := TIntTree.Create(1, nil);
  it2 := it1.Clone;
  WriteLn('ClassName of original instance: ', it1.ClassName);
  WriteLn('ClassName of cloned instance: ', it2.ClassName);
end.


