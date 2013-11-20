program demo3;
{$mode objfpc}{$H+}
uses types, Classes, sysutils, tree;
type
  TIntTree = class(specialize TTree<Integer>)
  protected
    procedure DoRestore(Ptr: Pointer; Size: QWord); override;
  end;
procedure Dump(t: TIntTree);
begin
  while t <> nil do begin
    WriteLn(Format('%s%d', [StringOfChar(' ', t.Level * 2), t.Data]));
    t := TIntTree(t.Next);
  end;
end;

var
  fs: TFileStream;
  it, it2: TIntTree;

procedure TIntTree.DoRestore(Ptr: Pointer; Size: QWord);
begin
  inherited DoRestore(Ptr, Size);
  if Data mod 2 = 0 then Data := Data * 5 else Data := Data * 10;
end;

begin
  it := TIntTree.Create(0, nil);
  TIntTree.Create(11, TIntTree.Create(1, it));
  TIntTree.Create(22, TIntTree.Create(21, TIntTree.Create(2, it)).Parent);
  TIntTree.Create(33, TIntTree.Create(32, TIntTree.Create(31, TIntTree.Create(3, it)).Parent).Parent);
  Dump(it);
  fs := TFileStream.Create('demo.dump', fmCreate or fmOpenWrite);
  WriteLn('Saving Last Child, Saved: ', it.LastChild.Save(fs));
  fs.Seek(0, soFromBeginning);
  WriteLn('Loading to First Child, Loaded: ', it.FirstChild.Load(fs));
  Dump(it);
  fs.Free;
  fs := TFileStream.Create('demo2.dump', fmCreate or fmOpenWrite);
  WriteLn('Saving changed tree, Saved: ', it.Save(fs));
  fs.Seek(0, soFromBeginning);
  it2 := TIntTree.Create(0, nil);
  WriteLn('Loading saved tree, Loaded: ', it2.Load(fs));
  Dump(it2);
  fs.Free;
  it.Free;
  it2.Free;
end.

