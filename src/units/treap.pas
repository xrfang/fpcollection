unit treap;
{$mode objfpc}
interface
uses contnrs;
type
  generic TTreap<TKey, TValue> = class
  public type
    PNode = ^TNode;
    TNode = record
      Count: Cardinal;
      Key: TKey;
      Value: TValue;
      Priority: Cardinal;
      Left, Right: PNode;
    end;
    TComparator = function(Key: TKey; Node: PNode): Integer of object;
  private const
    MAX_PRIORITY = $7FFFFFFF;
  private
    NullNode, RootNode: PNode;
    Altered: Boolean;   //for Insert & Delete
    ProxyFor: TTreap;   //for enumeration
    Direction: Integer; //for enumeration
    StartIdx: Integer;  //for enumeration
    StartNode: PNode;   //for enumeration
    NodeCount: Integer; //for enumeration
    Enums: TStack;      //for enumeration
    FCurrent: PNode;    //for enumeration
    FComparator: TComparator;
    function GetCount: Integer;
    procedure SetCount(Node: PNode);
    function LeftRotate(Node: PNode): PNode;
    function RightRotate(Node: PNode): PNode;
    function InsertNode(Key: TKey; Value: TValue; Node: PNode): PNode;
    function DeleteNode(Key: TKey; Node: PNode): PNode;
    procedure ClearNode(Node: PNode);
  protected
    function DefaultComparator(Key: TKey; Node: PNode): Integer; virtual;
    procedure Disposer(Node: PNode); virtual;
  public
    property Comparator: TComparator read FComparator write FComparator;
    property Count: Integer read GetCount;
    property Current: PNode read FCurrent;
    function GetEnumerator: TTreap;
    function Range(ACount: Integer; AStart: Integer = 0): TTreap;
    function Reversed: TTreap;
    function MoveNext: Boolean;
    function Insert(Key: TKey; Value: TValue): Boolean;
    function Delete(Key: TKey): Boolean;
    function Find(Key: TKey; PRank: PCardinal = nil): PNode;
    function Fetch(Rank: Integer): PNode;
    function Rank(Key: TKey): Cardinal;
    function ValueAt(ARank: Integer; ADefault: TValue): TValue;
    function ValueOf(Key: TKey; ADefault: TValue): TValue;
    procedure Import(src: TTreap; Clean: Boolean = True);
    procedure Clear;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

function TTreap.InsertNode(Key: TKey; Value: TValue; Node: PNode): PNode;
begin
  if Node = NullNode then begin
    New(Node);
    Node^.Key := Key;
    Node^.Value := Value;
    Node^.Count := 1;
    Node^.Priority := Random(MAX_PRIORITY);
    Node^.Left := NullNode;
    Node^.Right := NullNode;
    Altered := True;
  end else begin
    case FComparator(Key, Node) of
      0: begin
        if Node^.Value <> Value then begin
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
    case FComparator(Key, Node) of
      0: begin
        if Node^.Left^.Priority < Node^.Right^.Priority then
          Node := LeftRotate(Node)
        else
          Node := RightRotate(Node);
        if Node <> NullNode then
          Node := DeleteNode(Key, Node)
        else begin
          Disposer(Node^.Left);
          Dispose(Node^.Left);
          Node^.Left := NullNode;
          SetCount(Node);
          Altered := True;
        end;
      end;
      1: begin
        Node^.Right := DeleteNode(Key, Node^.Right);
        SetCount(Node);
      end
      else begin
        Node^.Left := DeleteNode(Key, Node^.Left);
        SetCount(Node);
      end;
    end;
  end;
  Result := Node;
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
  FComparator := @DefaultComparator;
  Enums := nil;
  Direction := 1;
  NodeCount := 0;
  StartIdx := 0;
end;

destructor TTreap.Destroy;
begin
  if ProxyFor = nil then begin
    Clear;
    Dispose(NullNode);
  end;
  if Enums <> nil then Enums.Free;
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

function TTreap.Find(Key: TKey; PRank: PCardinal): PNode;
var
  Anchor: TKey;
  r: Cardinal;
begin
  Result := RootNode;
  r := Result^.Count - Result^.Right^.Count;
  repeat
    Anchor := Result^.Key;
    case FComparator(Key, Result) of
      -1: begin
        Result := Result^.Left;
        r := r - Result^.Count + Result^.Left^.Count;
      end;
      1: begin
        Result := Result^.Right;
        r := r + Result^.Count - Result^.Right^.Count;
      end;
      else Break;
    end;
  until Result = NullNode;
  if Result = NullNode then begin
    if (r = 0) or (Anchor < Key) then r += 1;
    Result := nil;
  end;
  if PRank <> nil then PRank^ := r;
end;

function TTreap.Fetch(Rank: Integer): PNode;
var
  r: Cardinal;
begin
  if Rank < 0 then Rank := Count + 1 + Rank;
  if (Rank <= 0) or (Rank > Count) then Exit(nil);
  Result := RootNode;
  r := Result^.Count - Result^.Right^.Count;
  while Result <> NullNode do begin
    if Rank = r then Break;
    if Rank < r then begin
      Result := Result^.Left;
      r := r - Result^.Count + Result^.Left^.Count;
    end else begin
      Result := Result^.Right;
      r := r + Result^.Count - Result^.Right^.Count;
    end;
  end;
  if Result = NullNode then Result := nil;
end;

function TTreap.Rank(Key: TKey): Cardinal;
var
  n: PNode;
begin
  n := Find(Key, @Result);
  if n = nil then Result := 0;
end;

function TTreap.ValueAt(ARank: Integer; ADefault: TValue): TValue;
var
  n: PNode;
begin
  n := Fetch(ARank);
  if n = nil then Exit(ADefault);
  Exit(n^.Value);
end;

function TTreap.ValueOf(Key: TKey; ADefault: TValue): TValue;
var
  n: PNode;
begin
  n := Find(Key);
  if n = nil then Exit(ADefault);
  Exit(n^.Value);
end;

procedure TTreap.Import(src: TTreap; Clean: Boolean);
var
  n: PNode;
begin
  if Clean then Clear;
  for n in src do Insert(n^.Key, n^.Value);
end;

procedure TTreap.Clear;
begin
  ClearNode(RootNode);
  RootNode := NullNode;
end;

function TTreap.DefaultComparator(Key: TKey; Node: PNode): Integer;
begin
  if Key < Node^.Key then
    Result := -1
  else if Key > Node^.Key then
    Result := 1
  else
    Result := 0;
end;

procedure TTreap.Disposer(Node: PNode);
begin
end;

function TTreap.GetEnumerator: TTreap;
begin
  Result := TTreap.Create;
  Result.ProxyFor := Self;
  Result.Comparator := FComparator;
  Result.Direction := Direction;
  Result.Enums := TStack.Create;
  Result.Enums.Push(RootNode);
  Result.NodeCount := NodeCount;
  Dispose(Result.NullNode);
  Result.NullNode := NullNode;
  if StartIdx <> 0 then begin
    Result.StartNode := Fetch(StartIdx);
    if Result.StartNode = nil then Result.Direction := 0;
  end else Result.StartNode := Fetch(Direction);
  Direction := 1;
  NodeCount := 0;
  StartIdx := 0;
end;

function TTreap.Range(ACount: Integer; AStart: Integer): TTreap;
begin
  StartIdx := AStart;
  NodeCount := ACount + 1;
  Result := Self;
end;

function TTreap.Reversed: TTreap;
begin
  Direction := -1;
  Result := Self;
end;

function TTreap.MoveNext: Boolean;
var
  Node: PNode;
  d: Integer;
begin
  if (ProxyFor = nil) or (Direction = 0) then Exit(False);
  while Enums.Count > 0 do begin
    Node := Enums.Pop;
    if Node = NullNode then Break;
    if Node = nil then begin
      Dec(NodeCount);
      if NodeCount <> 0 then begin
        FCurrent := Enums.Pop;
        Exit(True);
      end else Break;
    end else begin
      d := 0;
      if StartNode <> nil then d := FComparator(Node^.Key, StartNode);
      if Direction > 0 then begin
        if Node^.Right <> NullNode then Enums.Push(Node^.Right);
        if d >= 0 then begin
          Enums.Push(Node);
          Enums.Push(nil);
          if Node^.Left <> NullNode then Enums.Push(Node^.Left);
        end;
      end else begin
        if Node^.Left <> NullNode then Enums.Push(Node^.Left);
        if d <= 0 then begin
          Enums.Push(Node);
          Enums.Push(nil);
          if Node^.Right <> NullNode then Enums.Push(Node^.Right);
        end;
      end;
    end;
  end;
  Result := False;
end;
end.
