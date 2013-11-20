unit tree;
{$mode objfpc}{$H+}
interface
uses sysutils, Classes;
type
  generic TTree<T> = class
  protected type
    TSelfType = TTree;
    TSelfClass = class of TSelfType;
  private
    FItems: TFPList;
    FParent: TTree;
    function ReadNodeData(s: TStream; out lev: QWord; out ptr: Pointer;
      out cnt: QWord): Boolean;
  protected
    function RLEncode(Value: QWord; Output: TStream): Integer;
    function RLDecode(Input: TStream; out Value: QWord): Integer;
    procedure DoClone(Source: TSelfType; var Target: TSelfType); virtual;
    function DoPersist(out Ptr: Pointer): Integer; virtual;
    procedure DoRestore(Ptr: Pointer; Size: QWord); virtual;
  public
    Data: T;
    property Parent: TTree read FParent;
    constructor Create(AData: T; AParent: TTree; APos: Integer = -1); virtual;
    destructor Destroy; override;
    function ChildAt(Rank: Cardinal): TTree;
    function Children: Cardinal;
    procedure Clear;
    function Clone: TTree;
    function Descendants: Cardinal;
    function FirstChild: TTree;
    function FirstDescendant: TTree;
    function FirstSibling: TTree;
    function LastChild: TTree;
    function LastDescendant: TTree;
    function LastSibling: TTree;
    function Level: Cardinal;
    function Load(s: TStream): Integer;
    function Next: TTree;
    function NextSibling: TTree;
    function Previous: TTree;
    function PreviousSibling: TTree;
    function Rank: Cardinal;
    function Remove(ANewParent: TTree = nil; APos: Integer = -1): TTree;
    function Root: TTree;
    function Save(s: TStream): Integer;
  end;

implementation

function TTree.Root: TTree;
begin
  Result := Self;
  while Result.Parent <> nil do Result := Result.Parent;
end;

function TTree.Save(s: TStream): Integer;
var
  node: TTree;
  c: Integer;
  p: Pointer;
begin
  Result := 0;
  node := Self;
  repeat
    Inc(Result);
    c := node.DoPersist(p);
    if c > 0 then begin
      RLEncode(node.Level - Level, s);
      RLEncode(c, s);
      s.WriteBuffer(p^, c);
      FreeMem(p, c);
    end;
    node := node.Next;
  until (node = nil) or (node.Level <= Level);
end;

function TTree.Level: Cardinal;
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

function TTree.ReadNodeData(s: TStream; out lev: QWord; out ptr: Pointer; out
  cnt: QWord): Boolean;
begin
  if (RLDecode(s, lev) <= 0) or (RLDecode(s, cnt) <= 0) then Exit(False);
  ptr := GetMem(cnt);
  s.ReadBuffer(ptr^, cnt);
  lev := lev + Level;
  Result := True;
end;

function TTree.Load(s: TStream): Integer;
var
  lv, c: QWord;
  node: TTree;
  buf: Pointer;
begin
  Clear;
  if not ReadNodeData(s, lv, buf, c) then Exit(0);
  DoRestore(buf, c);
  FreeMem(buf, c);
  Result := 1;
  node := Self;
  while ReadNodeData(s, lv, buf, c) do begin
    while (node <> nil) and (lv < node.Level) do node := node.Parent;
    if (lv = node.Level) and (node <> Self) then
      node := TSelfClass(Self.ClassType).Create(Data, node.Parent)
    else
      node := TSelfClass(Self.ClassType).Create(Data, node);
    node.DoRestore(buf, c);
    FreeMem(buf, c);
    Inc(Result);
  end;
end;

function TTree.Children: Cardinal;
begin
  Result := FItems.Count;
end;

function TTree.Clone: TTree;
var
  nsrc, ndup: TTree;
begin
  Result := TSelfClass(Self.ClassType).Create(Data, FParent);
  nsrc := FirstChild;
  while nsrc <> nil do begin
    ndup := nsrc.Clone;
    if ndup <> nil then ndup.Remove(Result);
    nsrc := nsrc.NextSibling;
  end;
  DoClone(Self, Result);
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

function TTree.FirstDescendant: TTree;
var
  p: TTree;
begin
  p := Self;
  while True do if p.FirstChild = nil then Exit(p) else p := p.FirstChild;
end;

function TTree.FirstSibling: TTree;
begin
  if FParent = nil then Exit(nil);
  Result := FParent.FirstChild;
end;

function TTree.Rank: Cardinal;
begin
  if FParent = nil then Exit(0) else Exit(FParent.FItems.IndexOf(Self) + 1);
end;

function TTree.LastDescendant: TTree;
var
  p: TTree;
begin
  p := Self;
  while True do if p.LastChild = nil then Exit(p) else p := p.LastChild;
end;

function TTree.LastChild: TTree;
begin
  if FItems.Count = 0 then Exit(nil);
  Exit(TSelfType(FItems[FItems.Count - 1]));
end;

function TTree.LastSibling: TTree;
begin
  if FParent = nil then Exit(nil);
  Result := FParent.LastChild;
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

procedure TTree.Clear;
begin
  while LastChild <> nil do LastChild.Free;
end;

function TTree.RLEncode(Value: QWord; Output: TStream): Integer;
var
  d: byte;
begin
  Result := 0;
  repeat
    d := Value mod 128;
    Value := Value div 128;
    if Value > 0 then d := d or $80;
    Output.Write(d, 1);
    Inc(Result);
  until Value = 0;
end;

function TTree.RLDecode(Input: TStream; out Value: QWord): Integer;
var
  d: Byte;
  multiplier: QWord;
  Complete: Boolean;
begin
  Result := 0;
  Value := 0;
  multiplier := 1;
  Complete := False;
  while Input.Read(d, 1) > 0 do begin
    Inc(Result);
    Value := Value + (d and $7F) * multiplier;
    multiplier := multiplier * 128;
    if d and $80 = 0 then begin
      Complete := True;
      Break;
    end;
  end;
  if not Complete then Result := -Result;
end;

procedure TTree.DoClone(Source: TSelfType; var Target: TSelfType);
begin
  (* empty *)
end;

function TTree.DoPersist(out Ptr: Pointer): Integer;
begin
  Result := SizeOf(T);
  Ptr := GetMem(Result);
  Move(Data, Ptr^, Result);
end;

procedure TTree.DoRestore(Ptr: Pointer; Size: QWord);
begin
  Data := T(Ptr^);
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
begin
  Remove;
  with FItems do while Count > 0 do TTree(FItems[Count - 1]).Free;
  FItems.Free;
end;

function TTree.ChildAt(Rank: Cardinal): TTree;
var
  idx: Integer;
begin
  idx := Rank - 1;
  if (idx < 0) or (idx >= FItems.Count) then Exit(nil);
  Exit(TSelfType(FItems[idx]));
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
