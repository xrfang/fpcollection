unit treap;
{$mode objfpc}
interface
type

  { TTreap }

  generic TTreap<TKey, TValue> = class
    const
      MAX_PRIORITY = $7FFFFFFF;
    type
      PNode = ^TNode;
      TNode = record
        Count: Cardinal;
        Key: TKey;
        Value: TValue;
        Priority: Cardinal;
        Left, Right: PNode;
      end;
    private
      Altered: Boolean; //for Insert & Delete
      GoOn: Boolean;    //for Traverse
      NullNode, RootNode: PNode;
      function GetCount: Integer;
      procedure SetCount(Node: PNode);
      function GetNode(Key: TKey): PNode;
      function LeftRotate(Node: PNode): PNode;
      function RightRotate(Node: PNode): PNode;
      function InsertNode(Key: TKey; Value: TValue; Node: PNode): PNode;
      function DeleteNode(Key: TKey; Node: PNode): PNode;
      procedure TraverseNode(Node: PNode; dir: Integer);
      procedure ClearNode(Node: PNode);
    protected
      function Traverse({%H-}Key: TKey; {%H-}Value: TValue): Boolean; virtual;
      function OnCompare({%H-}Key: TKey; Node: PNode): Integer; virtual;
      procedure OnDispose({%H-}Value: TValue); virtual;
      function OnInsert({%H-}Key: TKey; {%H-}Value: TValue; {%H-}IsNew: Boolean): Boolean; virtual;
    public
      property Count: Integer read GetCount;
      property Item[Key: TKey]: PNode read GetNode; default;
      function Insert(Key: TKey; Value: TValue): Boolean;
      function Delete(Key: TKey): Boolean;
      function Find(Key: TKey; out Rank: Cardinal): PNode;
      function Fetch(Rank: Cardinal): PNode;
      procedure Clear;
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Walk(dir: Integer = 0); //0=ascending; 1=descending
  end;

implementation

function TTreap.InsertNode(Key: TKey; Value: TValue; Node: PNode): PNode;
begin
  if Node = NullNode then begin
    if OnInsert(Key, Value, True) then begin
      New(Node);
      Node^.Key := Key;
      Node^.Value := Value;
      Node^.Count := 1;
      Node^.Priority := Random(MAX_PRIORITY);
      Node^.Left := NullNode;
      Node^.Right := NullNode;
      Altered := True;
    end;
  end else begin
    case OnCompare(Key, Node) of
      0: begin
        if (Node^.Value <> Value) and OnInsert(Key, Value, False) then begin
          Node^.Value := Value;
          Altered := True;
        end;
      end;
      1: begin
        Node^.Right := InsertNode(Key, Value, Node^.Right);
        SetCount(Node);
        if Node^.Right^.Priority < Node^.Priority then
          Node := RightRotate(Node);
      end;
      else begin
        Node^.Left := InsertNode(Key, Value, Node^.Left);
        SetCount(Node);
        if Node^.Left^.Priority < Node^.Priority then
          Node := LeftRotate(Node);
      end;
    end;
  end;
  Result := Node;
end;

function TTreap.DeleteNode(Key: TKey; Node: PNode): PNode;
begin
  if Node <> NullNode then begin
    case OnCompare(Key, Node) of
      0: begin
        if Node^.Left^.Priority < Node^.Right^.Priority then
          Node := LeftRotate(Node)
        else
          Node := RightRotate(Node);
        if Node <> NullNode then
          Node := DeleteNode(Key, Node)
        else begin
          OnDispose(Node^.Left^.Value);
          Dispose(Node^.Left);
          Node^.Left := NullNode;
          SetCount(Node);
          Altered := True;
        end;
      end;
      1: Node^.Right := DeleteNode(Key, Node^.Right);
      else Node^.Left := DeleteNode(Key, Node^.Left);
    end;
  end;
  Result := Node;
end;

procedure TTreap.TraverseNode(Node: PNode; dir: Integer);
begin
  if (not GoOn) or (Node = NullNode) then Exit;
  if dir = 0 then begin
    TraverseNode(Node^.Left, dir);
    if GoOn then GoOn := Traverse(Node^.Key, Node^.Value);
    TraverseNode(Node^.Right, dir);
  end else begin
    TraverseNode(Node^.Right, dir);
    if GoOn then GoOn := Traverse(Node^.Key, Node^.Value);
    TraverseNode(Node^.Left, dir);
  end;
end;

procedure TTreap.ClearNode(Node: PNode);
begin
  if Node^.Left <> NullNode then begin
    ClearNode(Node^.Left);
    Node^.Left := NullNode;
  end;
  if Node^.Right <> NullNode then begin
    ClearNode(Node^.Right);
    Node^.Right := NullNode;
  end;
  DeleteNode(Node^.Key, Node);
end;

function TTreap.LeftRotate(Node: PNode): PNode;
begin
  Result := Node^.Left;
  Node^.Left := Result^.Right;
  SetCount(Node);
  Result^.Right := Node;
  SetCount(Result);
end;

function TTreap.GetCount: Integer;
begin
  Result := RootNode^.Count;
end;

procedure TTreap.SetCount(Node: PNode);
begin
  if Node = NullNode then Exit;
  Node^.Count := Node^.Left^.Count + Node^.Right^.Count + 1;
end;

function TTreap.GetNode(Key: TKey): PNode;
var
  r: Cardinal;
begin
  Result := Find(Key, r);
end;

function TTreap.RightRotate(Node: PNode): PNode;
begin
  Result := Node^.Right;
  Node^.Right := Result^.Left;
  SetCount(Node);
  Result^.Left := Node;
  SetCount(Result);
end;

constructor TTreap.Create;
begin
  Randomize;
  New(NullNode);
  NullNode^.Count := 0;
  NullNode^.Left := NullNode;
  NullNode^.Right := NullNode;
  NullNode^.Priority := MAX_PRIORITY;
  RootNode := NullNode;
end;

destructor TTreap.Destroy;
begin
  Clear;
  Dispose(NullNode);
end;

procedure TTreap.Walk(dir: Integer);
begin
  GoOn := True;
  TraverseNode(RootNode, dir);
end;

function TTreap.Insert(Key: TKey; Value: TValue): Boolean;
begin
  Altered := False;
  RootNode := InsertNode(Key, Value, RootNode);
  Result := Altered;
end;

function TTreap.Delete(Key: TKey): Boolean;
begin
  Altered := False;
  RootNode := DeleteNode(Key, RootNode);
  Result := Altered;
end;

function TTreap.Find(Key: TKey; out Rank: Cardinal): PNode;
var
  Anchor: TKey;
begin
  Result := RootNode;
  Rank := Result^.Count - Result^.Right^.Count;
  repeat
    Anchor := Result^.Key;
    case OnCompare(Key, Result) of
      -1: begin
        Result := Result^.Left;
        Rank := Rank - Result^.Count + Result^.Left^.Count;
      end;
      1: begin
        Result := Result^.Right;
        Rank := Rank + Result^.Count - Result^.Right^.Count;
      end;
      else Break;
    end;
  until Result = NullNode;
  if Result = NullNode then begin
    if (Rank = 0) or (Anchor < Key) then Rank += 1;
    Result := nil;
  end;
end;

function TTreap.Fetch(Rank: Cardinal): PNode;
var
  r: Cardinal;
begin
  Result := RootNode;
  r := Result^.Count - Result^.Right^.Count;
  while Result <> NullNode do begin
    if Rank = r then Exit;
    if Rank < r then begin
      Result := Result^.Left;
      r := r - Result^.Count + Result^.Left^.Count;
    end else begin
      Result := Result^.Right;
      r := r + Result^.Count - Result^.Right^.Count;
    end;
  end;
end;

procedure TTreap.Clear;
begin
  ClearNode(RootNode);
  RootNode := NullNode;
end;

function TTreap.OnCompare(Key: TKey; Node: PNode): Integer;
begin
  if Key < Node^.Key then
    Result := -1
  else if Key > Node^.Key then
    Result := 1
  else
    Result := 0;
end;

function TTreap.Traverse(Key: TKey; Value: TValue): Boolean;
begin
  Result := True;
end;

procedure TTreap.OnDispose(Value: TValue);
begin
end;

function TTreap.OnInsert(Key: TKey; Value: TValue; IsNew: Boolean): Boolean;
begin
  Result := True;
end;

end.
