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
        Key: TKey;
        Value: TValue;
        Priority: Cardinal;
        Left, Right: PNode;
      end;
    private
      Altered: Boolean; //for Insert & Delete
      GoOn: Boolean;    //for Traverse
      FCount: Integer;
      NullNode, RootNode: PNode;
      function GetCount: Integer;
      function LeftRotate(Node: PNode): PNode;
      function RightRotate(Node: PNode): PNode;
      function InsertNode(Key: TKey; Value: TValue; Node: PNode): PNode;
      function DeleteNode(Key: TKey; Node: PNode): PNode;
      procedure TraverseNode(Node: PNode; dir: Integer; UserData: Pointer);
      procedure ClearNode(Node: PNode);
      function Compare({%H-}Key: TKey; Node: PNode): Integer;
    protected
      function Traverse({%H-}Key: TKey; {%H-}Value: TValue; {%H-}UserData: Pointer): Boolean; virtual;
      procedure OnDispose({%H-}Value: TValue); virtual;
    public
      property Count: Integer read GetCount;
      function Insert(Key: TKey; Value: TValue): Boolean;
      function Delete(Key: TKey): Boolean;
      function Find(Key: TKey): PNode;
      procedure Clear;
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Walk(dir: Integer = 0; UserData: Pointer = nil);
  end;

implementation

function TTreap.InsertNode(Key: TKey; Value: TValue; Node: PNode): PNode;
begin
  if Node = NullNode then begin
    New(Node);
    Node^.Key := Key;
    Node^.Value := Value;
    Node^.Priority := Random(MAX_PRIORITY);
    Node^.Left := NullNode;
    Node^.Right := NullNode;
    Altered := True;
    Inc(FCount);
  end else begin
    case Compare(Key, Node) of
      0: begin
        Altered := Node^.Value <> Value;
        Node^.Value := Value;
      end;
      1: begin
        Node^.Right := InsertNode(Key, Value, Node^.Right);
        if Node^.Right^.Priority < Node^.Priority then
          Node := RightRotate(Node);
      end;
      else begin
        Node^.Left := InsertNode(Key, Value, Node^.Left);
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
    case Compare(Key, Node) of
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
          Altered := True;
          Dec(FCount);
        end;
      end;
      1: Node^.Right := DeleteNode(Key, Node^.Right);
      else Node^.Left := DeleteNode(Key, Node^.Left);
    end;
  end;
  Result := Node;
end;

procedure TTreap.TraverseNode(Node: PNode; dir: Integer; UserData: Pointer);
begin
  if (not GoOn) or (Node = NullNode) then Exit;
  if dir = 0 then begin
    TraverseNode(Node^.Left, dir, UserData);
    if GoOn then GoOn := Traverse(Node^.Key, Node^.Value, UserData);
    TraverseNode(Node^.Right, dir, UserData);
  end else begin
    TraverseNode(Node^.Right, dir, UserData);
    if GoOn then GoOn := Traverse(Node^.Key, Node^.Value, UserData);
    TraverseNode(Node^.Left, dir, UserData);
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
  Result^.Right := Node;
end;

function TTreap.GetCount: Integer;
begin
  Result := FCount;
end;

function TTreap.RightRotate(Node: PNode): PNode;
begin
  Result := Node^.Right;
  Node^.Right := Result^.Left;
  Result^.Left := Node;
end;

constructor TTreap.Create;
begin
  Randomize;
  New(NullNode);
  NullNode^.Left := NullNode;
  NullNode^.Right := NullNode;
  NullNode^.Priority := MAX_PRIORITY;
  RootNode := NullNode;
  FCount := 0;
end;

destructor TTreap.Destroy;
begin
  Clear;
end;

procedure TTreap.Walk(dir: Integer; UserData: Pointer);
begin
  GoOn := True;
  TraverseNode(RootNode, dir, UserData);
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

function TTreap.Find(Key: TKey): PNode;
var
  cmp : Integer;
begin
  Result := RootNode;
  while Result <> NullNode do begin
    cmp := Compare(Key, Result);
    if cmp < 0 then
      Result := Result^.Left
    else if cmp > 0 then
      Result := Result^.Right
    else
      Break;
  end;
  if Result = NullNode then Result := nil;
end;

procedure TTreap.Clear;
begin
  ClearNode(RootNode);
  RootNode := NullNode;
end;

function TTreap.Compare(Key: TKey; Node: PNode): Integer;
begin
  if Key < Node^.Key then
    Result := -1
  else if Key > Node^.Key then
    Result := 1
  else
    Result := 0;
end;

function TTreap.Traverse(Key: TKey; Value: TValue; UserData: Pointer): Boolean;
begin
  Result := True;
end;

procedure TTreap.OnDispose(Value: TValue);
begin
end;

end.
