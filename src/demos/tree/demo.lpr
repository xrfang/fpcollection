program demo;
{$mode objfpc}{$H+}
uses
  Classes, sysutils, tree;
type
  TStringTree = specialize TTree<string>;

procedure PrintTree(root: TStringTree);
begin
  WriteLn('Iterating through all nodes...');
  while root <> nil do begin
    WriteLn(Format('%s[%d] %s', [StringOfChar(' ', root.Level * 2), root.Rank, root.Data]));
    root := root.Next;
  end;
end;

var
  root, tr, tr2: TStringTree;
begin
  WriteLn('Populating tree with demo data.');
  tr := TStringTree.Create('root', nil);
  tr := TStringTree.Create('n1', tr);
  TStringTree.Create('n11', tr);
  tr := TStringTree.Create('n12', tr);
  TStringTree.Create('n121', tr);
  tr := tr.Root;
  TStringTree.Create('n3', tr);
  WriteLn('Now we have n1 and n3 added as children of ROOT.');
  PrintTree(tr);
  WriteLn('Clone the tree to a new one.');
  tr2 := tr.Clone;
  PrintTree(tr2);
  tr2.Free;
  WriteLn('Freed the cloned tree.');
  WriteLn('Insert n2 to be the second child of ROOT...');
  TStringTree.Create('n21', TStringTree.Create('n2', tr, 1));
  WriteLn('Clone node n2...');
  tr := tr.Root.FirstChild.NextSibling;
  WriteLn('Confirm: node to clone is "', tr.Data, '"');
  tr.Clone;
  PrintTree(tr.Root);
  WriteLn('Number of nodes in the tree: ', tr.Root.Descendants + 1);
  tr := tr.Root.LastChild;
  WriteLn('Purge Children of node: ', tr.Data);
  tr.PurgeChildren;
  PrintTree(tr.Root);
  root := tr.Root;
  tr := tr.Root.FirstChild.NextSibling;
  WriteLn('Purge second child of root node: ', tr.Data);
  tr.Free;
  PrintTree(root);
  tr := root.FirstChild;
  WriteLn('Last descendant of node "', tr.Data, '" is: ',
    tr.LastDescendant.Data, ', whose rank is: ', tr.LastDescendant.Rank);
  WriteLn('First sibling of node "', tr.Data, '" is: ', tr.FirstSibling.Data,
    ', whose rank is: ', tr.FirstSibling.Rank);
  WriteLn('Last sibling of node "', tr.Data, '" is: ', tr.LastSibling.Data,
    ', whose rank is: ', tr.LastSibling.Rank);
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
  WriteLn('Move node n12 to be the second child of ROOT...');
  tr := root.FirstChild.FirstChild.NextSibling;
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

