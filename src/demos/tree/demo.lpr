program demo;
{$mode objfpc}{$H+}
uses
  Classes, tree;
type
  TStringTree = specialize TTree<string>;

procedure PrintTree(root: TStringTree);
begin
  WriteLn('Iterating through all nodes...');
  while root <> nil do begin
    WriteLn(StringOfChar(' ', root.Level * 2), root.Data);
    root := root.Next;
  end;
end;

var
  root, tr: TStringTree;
begin
  WriteLn('Populating tree with demo data.');
  tr := TStringTree.Create('root', nil);
  tr := TStringTree.Create('n1', tr);
  TStringTree.Create('n11', tr);
  tr := TStringTree.Create('n12', tr);
  TStringTree.Create('n121', tr);
  tr := tr.Root;
  TStringTree.Create('n21', TStringTree.Create('n2', tr));
  TStringTree.Create('n3', tr);
  WriteLn('Number of nodes in the tree: ', tr.Root.Descendants + 1);
  WriteLn('Iterating through children of the ROOT node... ');
  root := tr.Root;
  tr := root;
  WriteLn('Number of children for the root node: ', tr.Children);
  tr := tr.Root.FirstChild;
  while tr <> nil do begin
    Write(string(tr.Data), #9);
    tr := tr.NextSibling;
  end;
  WriteLn;
  tr := root;
  PrintTree(tr);
  WriteLn('Prune node n12...');
  tr := root.FirstChild.FirstChild.Next;
  WriteLn('Confirm: node to prune is "', tr.Data, '"');
  tr.Remove.Free;
  tr := root;
  WriteLn('Number of nodes after prune is: ', tr.Descendants + 1);
  PrintTree(tr);
  WriteLn('Move node n21 to be the second child of ROOT...');
  tr := root.FirstChild.NextSibling.FirstChild;
  WriteLn('Confirm: node to move is "', tr.Data, '"');
  tr.Remove(root, 1);
  WriteLn('Number of children of ROOT is: ', root.Children);
  PrintTree(tr.Root);
  WriteLn('Iterating through all nodes in reverse order...');
  tr := root;
  while tr.LastChild <> nil do tr := tr.LastChild;
  while tr <> nil do begin
    WriteLn(StringOfChar(' ', tr.Level * 2), tr.Data);
    tr := tr.Previous;
  end;
  root.Free;
end.

