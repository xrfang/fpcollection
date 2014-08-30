program demo5;
uses heaptrc, sysutils, Classes, tree;
type
  TSLTree = class(specialize TTree<TStringList>)
  protected
    procedure DoDestroy; override;
  end;
procedure Dump(t: TSLTree; indent: string = '');
var
  i: Integer;
begin
  Write(indent, t.Data.Text);
  for i := 1 to t.Children do Dump(TSLTree(t.Child[i]), indent + '  ');
end;
var
  st: TSLTree;
  sl: TStringList;

procedure TSLTree.DoDestroy;
begin
  TStringList(Data).Free;
end;

begin
  DeleteFile('tree5.trc');
  SetHeapTraceOutput('tree5.trc');
  sl := TStringList.Create;
  sl.Text := 'StringList1';
  st := TSLTree.Create(sl, nil);
  sl := TStringList.Create;
  sl.Text := 'StringList2';
  st := TSLTree.Create(sl, st);
  sl := TStringList.Create;
  sl.Text := 'StringList3';
  st := TSLTree.Create(sl, st);
  while st.Parent <> nil do st := TSLTree(st.Parent);
  Dump(st);
  st.Free;
end.

