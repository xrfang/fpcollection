unit vector;
{$mode objfpc}{$H+}{$Inline ON}
interface
type
  generic TVector<T> = class
  type
    DataType = array of T;
  private
    FName: string;
    FOrder: Integer;
    FCapacity: Cardinal;
    FFirst, FLast: Integer;
    FData: DataType;
    FDefault: T;
    function GetCount: Integer;
    procedure AdjustHeadCapacity(Target: Integer);
    procedure AdjustTailCapacity(Target: Integer);
  protected
    function GetItem(Index: Integer): T; virtual;
    procedure SetItem(Index: Integer; AValue: T); virtual;
  public
    property Capacity: Cardinal read FCapacity;
    property Count: Integer read GetCount;
    property First: Integer read FFirst write FFirst;
    property Item[Index: Integer]: T read GetItem write SetItem; default;
    property Last: Integer read FLast write FLast;
    property MissingValue: T read FDefault write FDefault;
    property Name: string read FName write FName;
    property Raw: DataType read FData;
    constructor Create(ADefault: T; AName: string = '');
    destructor Destroy; override;
    function Pop: T;
    function Shift: T;
    procedure Assign(Values: DataType; Head: Integer = 0; Tail: Integer = -1);
    procedure Clear;
    procedure Push(AValue: T);
    procedure Trim;
    procedure Unshift(AValue: T);
  end;
  generic TSortableVector<T> = class(specialize TVector<T>)
  type
    TSwapAssoc = procedure(idx1, idx2: Integer) of object;
  private
    procedure SwapAssoc({%H-}idx1, {%H-}idx2: Integer); inline;
  protected
    function OnSort({%H-}v1, {%H-}v2: T): Integer; virtual;
  public
    function Max: T;
    function Min: T;
    procedure Sort(Reversed: Boolean = False; DoAssoc: TSwapAssoc = nil);
    procedure Swap(idx1, idx2: Integer); inline;
  end;
  TIntegerVector = specialize TSortableVector<Integer>;
  TDoubleVector = specialize TSortableVector<Double>;
  TObjectVector = specialize TVector<TObject>;
  TPointerVector = specialize TVector<Pointer>;
  TStringVector = specialize TSortableVector<string>;

implementation
uses math;

procedure TSortableVector.SwapAssoc(idx1, idx2: Integer);
begin
  (* do nothing *)
end;

procedure TSortableVector.Sort(Reversed: Boolean; DoAssoc: TSwapAssoc);
var
  i, gap, tail, pos: Integer;
begin
  if FLast <= FFirst then Exit;
  if Reversed then FOrder := 1 else FOrder := -1;
  if DoAssoc = nil then DoAssoc := @SwapAssoc;
  //comb sort for large gaps
  gap := FLast - FFirst + 1;
  while gap > 10 do begin
    gap := trunc(gap / 1.3);
    if gap in [9, 10] then gap := 11;
    for i := FFirst to FLast - gap do begin
      if FOrder * OnSort(FData[i], FData[i + gap]) < 0 then begin
        Swap(i, i + gap);
        DoAssoc(i - FFirst, i - FFirst + gap);
      end;
    end;
  end;
  //fallback to (optimized) gnome sort for small gaps
  pos := FFirst + 1;
  tail := FFirst;
  while pos <= FLast do begin
    if FOrder * OnSort(FData[pos - 1], FData[pos]) < 0 then begin
      Swap(pos, pos - 1);
      DoAssoc(pos - FFirst, pos - FFirst - 1);
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

function TSortableVector.OnSort(v1, v2: T): Integer;
begin
  if v1 < v2 then
    Result := -1
  else if v1 > v2 then
    Result := 1
  else
    Result := 0;
end;

function TSortableVector.Max: T;
var
  i: Integer;
begin
  if FLast < FFirst then  Exit(FDefault);
  if FOrder > 0 then      Exit(FData[FFirst])
  else if FOrder < 0 then Exit(FData[FLast])
  else begin
    Result := FData[FFirst];
    for i := FFirst+1 to FLast do if FData[i] > Result then Result := FData[i];
  end;
end;

function TSortableVector.Min: T;
var
  i: Integer;
begin
  if FLast < FFirst then  Exit(FDefault);
  if FOrder > 0 then      Exit(FData[FLast])
  else if FOrder < 0 then Exit(FData[FFirst])
  else begin
    Result := FData[FFirst];
    for i := FFirst+1 to FLast do if FData[i] < Result then Result := FData[i];
  end;
end;

procedure TSortableVector.Swap(idx1, idx2: Integer);
var
  Temp: T;
begin
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

constructor TVector.Create(ADefault: T; AName: string);
begin
  FDefault := ADefault;
  FName := AName;
  Clear;
end;

destructor TVector.Destroy;
begin
  Clear;
end;

procedure TVector.Push(AValue: T);
begin
  Item[FLast + 1] := AValue;
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
  FFirst := 0;
  FLast := -1;
  FCapacity := 0;
  FData := nil;
  FOrder := 0;
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
