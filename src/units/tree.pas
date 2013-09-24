unit tree;
{$mode objfpc}{$H+}
interface
uses Classes;
type
  generic TTree<T> = class
  public type
    TSelfType = TTree;
  private
    FItems: TFPList;
    FParent: TTree;
    function GetLevel: Cardinal;
  public
    Data: T;
    property Level: Cardinal read GetLevel;
    property Parent: TTree read FParent;
    constructor Create(AData: T; AParent: TTree; APos: Integer = -1);
    destructor Destroy; override;
    function Children: Cardinal;
    function Clone: TTree;
    function Descendants: Cardinal;
    function FirstChild: TTree;
    function LastChild: TTree;
    function Next: TTree;
    function NextSibling: TTree;
    function Previous: TTree;
    function PreviousSibling: TTree;
    function Remove(ANewParent: TTree = nil; APos: Integer = -1): TTree;
    function Root: TTree;
  end;

implementation

function TTree.Root: TTree;
begin
  Result := Self;
  while Result.Parent <> nil do Result := Result.Parent;
end;

function TTree.GetLevel: Cardinal;
var
  n: TTree;
begin
  Result := 0;
  n := Self.Parent;
  while n <> nil do begin
    Inc(Result);
    n := n.Parent;
  end;
end;

function TTree.Children: Cardinal;
begin
  Result := FItems.Count;
end;

function TTree.Clone: TTree;
var
  node: TTree;
begin
  Result := TTree.Create(Data, FParent);
  node := FirstChild;
  while node <> nil do begin
    node.Clone.Remove(Result);
    node := node.NextSibling;
  end;
end;

function TTree.Descendants: Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to FItems.Count - 1 do
    Result += 1 + TSelfType(FItems[i]).Descendants;
end;

function TTree.FirstChild: TTree;
begin
  if FItems.Count = 0 then Exit(nil);
  Exit(TSelfType(FItems[0]));
end;

function TTree.LastChild: TTree;
begin
  if FItems.Count = 0 then Exit(nil);
  Exit(TSelfType(FItems[FItems.Count - 1]));
end;

function TTree.Next: TTree;
var
  p: TTree;
begin
  if Children > 0 then Exit(FirstChild);
  p := Self;
  while True do begin
    Result := p.NextSibling;
    if Result <> nil then Exit;
    p := p.Parent;
    if p = nil then Exit(nil);
  end;
end;

function TTree.NextSibling: TTree;
var
  p: Integer;
begin
  if FParent = nil then Exit(nil);
  with FParent.FItems do begin
    p := IndexOf(Self);
    if p = Count - 1 then Exit(nil);
    Exit(TSelfType(Items[p + 1]));
  end;
end;

function TTree.Previous: TTree;
var
  lc: TTree;
begin
  Result := PreviousSibling;
  if Result = nil then Exit(FParent);
  while True do begin
    lc := Result.LastChild;
    if lc = nil then Exit;
    Result := lc;
  end;
end;

function TTree.PreviousSibling: TTree;
var
  p: Integer;
begin
  if FParent = nil then Exit(nil);
  with FParent.FItems do begin
    p := IndexOf(Self);
    if p = 0 then Exit(nil);
    Exit(TSelfType(Items[p - 1]));
  end;
end;

constructor TTree.Create(AData: T; AParent: TTree; APos: Integer);
begin
  Data := AData;
  FItems := TFPList.Create;
  FParent := AParent;
  if FParent <> nil then with FParent.FItems do
    if (APos >= 0) and (APos < Count) then Insert(APos, Self)
    else Add(Self);
end;

destructor TTree.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do TTree(FItems[i]).Free;
  FItems.Free;
end;

function TTree.Remove(ANewParent: TTree; APos: Integer): TTree;
begin
  if FParent <> nil then begin
    FParent.FItems.Remove(Self);
    FParent := nil;
  end;
  if ANewParent <> nil then begin
    FParent := ANewParent;
    with FParent.FItems do
      if (APos >= 0) and (APos < Count) then Insert(APos, Self)
      else Add(Self);
  end;
  Exit(Self);
end;

end.

