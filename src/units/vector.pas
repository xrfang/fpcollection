unit vector;
{$mode objfpc}{$H+}
interface
uses sysutils;
type
  EFrozenVector = class(Exception);
  generic TVector<T> = class
  type
    TSelfType = TVector;
    TSelfClass = class of TSelfType;
    DataType = array of T;
    SortOption = (soReversed, soEliminateNA);
    SortOptions = set of SortOption;
  private
    FCapacity: Integer;
    FCount: Integer;
    FData: DataType;
    FDefault: T;
    FFrozen: Boolean;
    function GetItem(Index: Integer): T;
    procedure SetFrozen(AValue: Boolean);
    procedure SetItem(Index: Integer; AValue: T);
    procedure AdjustCapacity(Usage: Integer);
  protected
    procedure OnUpdate(Index: Integer; OldValue: T; var NewValue: T); virtual;
    function OnSort(v1, v2: T): Integer; virtual;
  public
    property Capacity: Integer read FCapacity;
    property Count: Integer read FCount;
    property Frozen: Boolean read FFrozen write SetFrozen;
    property Item[Index: Integer]: T read GetItem write SetItem; default;
    property NA: T read FDefault;
    property Raw: DataType read FData;
    constructor Create(ADefault: T); virtual;
    destructor Destroy; override;
    function Clone: TVector;
    procedure Append(AValue: T);
    procedure Clear(Init: Boolean = False);
    procedure LTrim(AValue: Integer);
    procedure RTrim(AValue: Integer);
    procedure Sort(Options: SortOptions = []);
  end;

implementation
function TVector.GetItem(Index: Integer): T;
begin
  if Index < Count then Result := FData[Index] else Result := FDefault;
end;

procedure TVector.RTrim(AValue: Integer);
var
  i: Integer;
begin
  AValue := FCount - AValue;
  if AValue < 0 then AValue := 0;
  if FCount = AValue then Exit;
  if FFrozen then raise EFrozenVector.Create('cannot trim frozen vector');
  if AValue > FCount then begin
    AdjustCapacity(AValue);
    for i := FCount to AValue - 1 do FData[i] := FDefault;
  end;
  FCount := AValue;
end;

procedure TVector.Sort(Options: SortOptions);
var
  i, gap, order, first, last, pos: Integer;
  Temp: T;
begin
  if soReversed in Options then order := 1 else order := -1;
  //comb sort for large gaps
  gap := FCount;
  while gap > 10 do begin
    gap := trunc(gap / 1.3);
    if gap in [9, 10] then gap := 11;
    for i := 0 to FCount - 1 - gap do begin
      if ((soEliminateNA in Options) and (FData[i + gap] <> FDefault) and
          (FData[i] = FDefault)) or
         (order * OnSort(FData[i], FData[i + gap]) < 0) then begin
        Temp := FData[i];
        FData[i] := FData[i + gap];
        FData[i + gap] := Temp;
      end;
    end;
  end;
  //fallback to (optimized) gnome sort for small gaps
  pos := 1;
  last := 0;
  while pos < FCount do begin
    if ((soEliminateNA in Options) and (FData[pos] <> FDefault) and
        (FData[pos - 1] = FDefault)) or
       (order * OnSort(FData[pos - 1], FData[pos]) < 0) then begin
      Temp := FData[pos];
      FData[pos] := FData[pos - 1];
      FData[pos - 1] := Temp;
      if pos > 1 then begin
        if last = 0 then last := pos;
        Dec(pos);
      end else Inc(pos);
    end else begin
      if last <> 0 then begin
        pos := last;
        last := 0;
      end;
      Inc(pos);
    end;
  end;
  //eliminate N/A values
  if soEliminateNA in Options then begin
     first := 0;
     last := FCount - 1;
     if FData[last] <> FDefault then Exit;
     if FData[first] = FDefault then begin
       FCount := 0;
       Exit;
     end;
     while last > first do begin
       pos := (first + last) div 2;
       if FData[pos] = FDefault then last := pos - 1 else first := pos + 1;
     end;
     if FData[last] = FDefault then FCount := last else FCount := last + 1;
  end;
end;

procedure TVector.SetFrozen(AValue: Boolean);
begin
  if FFrozen = AValue then Exit;
  FFrozen := AValue;
  if FFrozen then AdjustCapacity(0) else AdjustCapacity(FCount);
end;

procedure TVector.SetItem(Index: Integer; AValue: T);
var
  i: Integer;
begin
  if Index >= FCount then begin
    if Index >= FCapacity then if FFrozen then
      raise EFrozenVector.Create('cannot expand frozen vector')
    else AdjustCapacity(Index + 1);
    for i := FCount to Index do FData[i] := FDefault;
    FCount := Index + 1;
  end;
  OnUpdate(Index, FData[Index], AValue);
  FData[Index] := AValue;
end;

procedure TVector.AdjustCapacity(Usage: Integer);
var
  n: Integer;
begin
  if Usage <= 0 then
    FCapacity := FCount
  else begin
    n := round(ln(Usage) / ln(2) + 0.5);
    FCapacity := 1 shl n;
  end;
  SetLength(FData, FCapacity);
end;

procedure TVector.OnUpdate(Index: Integer; OldValue: T; var NewValue: T);
begin
  (* empty *)
end;

function TVector.OnSort(v1, v2: T): Integer;
begin
  if v1 < v2 then
    Result := -1
  else if v1 > v2 then
    Result := 1
  else
    Result := 0;
end;

constructor TVector.Create(ADefault: T);
begin
  FDefault := ADefault;
  FFrozen := False;
  FCount := 0;
  FCapacity := 64; //allocate 64 byte initially.
  SetLength(FData, FCapacity);
end;

destructor TVector.Destroy;
begin
  FData := nil;
end;

function TVector.Clone: TVector;
begin
  Result := TSelfClass(Self.ClassType).Create(FDefault);
  Result.FCount := FCount;
  Result.FCapacity := FCapacity;
  Result.FFrozen := FFrozen;
  SetLength(Result.FData, FCapacity);
  Move(FData[0], Result.FData[0], FCount * SizeOf(T));
end;

procedure TVector.Append(AValue: T);
begin
  Item[FCount] := AValue;
end;

procedure TVector.Clear(Init: Boolean);
begin
  FCount := 0;
  if Init then begin
    FCapacity := 64;
    SetLength(FData, 64);
  end;
end;

procedure TVector.LTrim(AValue: Integer);
var
  i: Integer;
begin
  if AValue = 0 then Exit;
  if FFrozen then raise EFrozenVector.Create('cannot trim frozen vector');
  if AValue < 0 then begin
    AdjustCapacity(FCount - AValue);
    Move(FData[0], FData[-AValue], FCount * SizeOf(T));
    for i := 0 to -AValue - 1 do FData[i] := FDefault;
  end else begin
    Move(FData[AValue], FData[0], (FCount - AValue) * SizeOf(T));
  end;
  FCount := FCount - AValue;
end;

end.

