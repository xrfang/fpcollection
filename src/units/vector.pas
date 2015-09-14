unit vector;
{$mode objfpc}{$H+}{$Inline ON}
interface
uses types;
type
  PIntegerDynArray = ^TIntegerDynArray;
  generic TVector<T> = class
  type
    DataType = array of T;
  private
    FOrder: Integer;
    FCapacity: Cardinal;
    FFirst, FLast: Integer;
    FDefault: T;
    function GetCount: Integer;
    procedure AdjustHeadCapacity(Target: Integer);
    procedure AdjustTailCapacity(Target: Integer);
    procedure Initialize;
  protected
    FData: DataType;
    function GetItem(Index: Integer): T; virtual;
    procedure SetItem(Index: Integer; AValue: T); virtual;
  public
    property Capacity: Cardinal read FCapacity;
    property Count: Integer read GetCount;
    property First: Integer read FFirst write FFirst;
    property Item[Index: Integer]: T read GetItem write SetItem; default;
    property Last: Integer read FLast write FLast;
    property MissingValue: T read FDefault write FDefault;
    property Raw: DataType read FData;
    constructor Create(ADefault: T);
    destructor Destroy; override;
    function Dump(Head: Integer = 0; Tail: Integer = -1): DataType;
    function Pop: T;
    function Push(AValue: T): Integer;
    function Shift: T;
    procedure Assign(Values: DataType; Head: Integer = 0; Tail: Integer = -1);
    procedure Clear; virtual;
    procedure Trim;
    procedure Unshift(AValue: T);
  end;
  generic TSortableVector<T> = class(specialize TVector<T>)
  protected
    function OnSort({%H-}v1, {%H-}v2: T): Integer; virtual;
    procedure Swap(idx1, idx2: Integer; sync: PIntegerDynArray = nil); inline;
  public
    property Order: Integer read FOrder;
    procedure Sort(Reversed: Boolean = False; OldOrder: PIntegerDynArray = nil);
    procedure ReOrder(ord: TIntegerDynArray; Restore: Boolean = False);
  end;
  TIntegerVector = class(specialize TSortableVector<Integer>)
  protected
    function OnSort(v1, v2: Integer): Integer; override;
  end;
  TDoubleVector = class(specialize TSortableVector<Double>)
  protected
    function OnSort(v1, v2: Double): Integer; override;
  end;
  TDateTimeVector = class(specialize TSortableVector<TDateTime>)
  protected
    function OnSort(v1, v2: TDateTime): Integer; override;
  end;
  TStringVector = class(specialize TSortableVector<string>)
  protected
    function OnSort(v1, v2: string): Integer; override;
  end;
  TObjectVector = specialize TVector<TObject>;
  TPointerVector = specialize TVector<Pointer>;

implementation
uses math;

function TDoubleVector.OnSort(v1, v2: Double): Integer;
begin
  if v1 < v2 then
    Result := -1
  else if v1 > v2 then
    Result := 1
  else
    Result := 0;
end;

function TDateTimeVector.OnSort(v1, v2: TDateTime): Integer;
begin
  if v1 < v2 then
    Result := -1
  else if v1 > v2 then
    Result := 1
  else
    Result := 0;
end;

function TStringVector.OnSort(v1, v2: string): Integer;
begin
  if v1 < v2 then
    Result := -1
  else if v1 > v2 then
    Result := 1
  else
    Result := 0;
end;

function TIntegerVector.OnSort(v1, v2: Integer): Integer;
begin
  if v1 < v2 then
    Result := -1
  else if v1 > v2 then
    Result := 1
  else
    Result := 0;
end;

procedure TSortableVector.Sort(Reversed: Boolean; OldOrder: PIntegerDynArray);
var
  i, gap, tail, pos: Integer;
begin
  if FLast <= FFirst then Exit;
  if Reversed then FOrder := 1 else FOrder := -1;
  if OldOrder <> nil then begin
    SetLength(OldOrder^, FLast - FFirst + 1);
    for i := 0 to FLast - FFirst do OldOrder^[i] := i;
  end;
  //comb sort for large gaps
  gap := FLast - FFirst + 1;
  while gap > 10 do begin
    gap := trunc(gap / 1.3);
    if gap in [9, 10] then gap := 11;
    for i := FFirst to FLast - gap do begin
      if FOrder * OnSort(FData[i], FData[i + gap]) < 0 then
        Swap(i, i + gap, OldOrder);
    end;
  end;
  //fallback to (optimized) gnome sort for small gaps
  pos := FFirst + 1;
  tail := FFirst;
  while pos <= FLast do begin
    if FOrder * OnSort(FData[pos - 1], FData[pos]) < 0 then begin
      Swap(pos, pos - 1, OldOrder);
      if pos > FFirst + 1 then begin
        if tail = FFirst then tail := pos;
        Dec(pos);
      end else Inc(pos);
    end else begin
      if tail <> FFirst then begin
        pos := tail;
        tail := FFirst;
      end;
      Inc(pos);
    end;
  end;
end;

procedure TSortableVector.ReOrder(ord: TIntegerDynArray; Restore: Boolean);
var
  arr: DataType;
  i: Integer;
begin
  SetLength(arr, Length(ord));
  if Restore then for i := 0 to Length(ord) - 1 do arr[ord[i]] := Item[i]
  else for i := 0 to Length(ord) - 1 do arr[i] := Item[ord[i]];
  Assign(arr);
end;

function TSortableVector.OnSort(v1, v2: T): Integer;
begin
  Result := 0;
end;

procedure TSortableVector.Swap(idx1, idx2: Integer; sync: PIntegerDynArray);
var
  idx: Integer;
  Temp: T;
begin
  if sync <> nil then begin
    idx := sync^[idx1 - FFirst];
    sync^[idx1 - FFirst] := sync^[idx2 - FFirst];
    sync^[idx2 - FFirst] := idx;
  end;
  Temp := FData[idx1];
  FData[idx1] := FData[idx2];
  FData[idx2] := Temp;
end;

function TVector.GetCount: Integer;
begin
  Result := FLast - FFirst + 1;
end;

function TVector.GetItem(Index: Integer): T;
begin
  Index += FFirst;
  if (Index < FFirst) or (Index > FLast) then Exit(FDefault);
  Result := FData[Index];
end;

procedure TVector.SetItem(Index: Integer; AValue: T);
begin
  Index += FFirst;
  if Index < 0 then begin
    AdjustHeadCapacity(Index);
    Index := FFirst;
  end else if Index > FLast then AdjustTailCapacity(Index)
  else if Index < FFirst then FFirst := Index;
  FData[Index] := AValue;
  FOrder := 0;
end;

procedure TVector.AdjustHeadCapacity(Target: Integer);
var
  n: Cardinal;
  i: Integer;
begin
  n := 1 shl trunc(ln(FCapacity - Target + 1) / ln(2) + 1); //target is negative
  SetLength(FData, n);
  Move(FData[0], FData[n - FCapacity], FCapacity * SizeOf(T));
  for i := 0 to n - FCapacity - 1 do FData[i] := FDefault;
  FFirst := n - FCapacity + Target;
  FLast := FLast + n - FCapacity;
  FCapacity := n;
end;

procedure TVector.AdjustTailCapacity(Target: Integer);
var
  n: Cardinal;
  i: Integer;
begin
  n := 1 shl trunc(ln(Target + 1) / ln(2) + 1);
  if n <> FCapacity then begin
    FCapacity := n;
    SetLength(FData, FCapacity);
    for i := FLast + 1 to n - 1 do FData[i] := FDefault;
  end;
  FLast := Target;
end;

procedure TVector.Initialize;
begin
  FFirst := 0;
  FLast := -1;
  FCapacity := 0;
  FData := nil;
  FOrder := 0;
end;

constructor TVector.Create(ADefault: T);
begin
  Initialize;
  FDefault := ADefault;
end;

destructor TVector.Destroy;
begin
  Clear;
end;

function TVector.Dump(Head: Integer; Tail: Integer): DataType;
var
  len: Integer;
begin
  Head := Head + FFirst;
  if Tail < 0 then Tail := Tail + FLast else Tail := Tail + FFirst;
  len := Tail - Head + 1;
  if len <= 0 then Exit(nil);
  SetLength(Result, len);
  Move(FData[Head], Result[0], len * SizeOf(T));
end;

function TVector.Push(AValue: T): Integer;
begin
  Result := FLast + 1;
  AdjustTailCapacity(Result);
  FData[Result] := AValue;
  FOrder := 0;
end;

function TVector.Pop: T;
begin
  if FLast >= FFirst then begin
    Result := FData[FLast];
    Dec(FLast);
  end else Result := FDefault;
end;

function TVector.Shift: T;
begin
  if FFirst <= FLast then begin
    Result := FData[FFirst];
    Inc(FFirst);
  end else Result := FDefault;
end;

procedure TVector.Unshift(AValue: T);
begin
  Item[-1] := AValue;
end;

procedure TVector.Assign(Values: DataType; Head: Integer; Tail: Integer);
begin
  FCapacity := Length(Values);
  FFirst := Head;
  if Tail < 0 then FLast := Tail + FCapacity else FLast := Tail;
  SetLength(FData, FCapacity);
  Move(Values[0], FData[0], FCapacity * SizeOf(T));
end;

procedure TVector.Clear;
begin
  Initialize;
end;

procedure TVector.Trim;
begin
  FCapacity := FLast + 1;
  SetLength(FData, FCapacity);
  if FFirst = 0 then Exit;
  Move(FData[FFirst], FData[0], (FLast - FFirst + 1) * SizeOf(T));
  FLast -= FFirst;
  FCapacity -= FFirst;
  SetLength(FData, FCapacity);
  FFirst := 0;
end;

initialization
SetExceptionMask([
  exInvalidOp,
  exDenormalized,
  exZeroDivide,
  exOverflow,
  exUnderflow,
  exPrecision
]);
end.
