unit vector;
{$mode objfpc}{$H+}
interface
uses sysutils;
type
  EFrozenVector = class(Exception);
  generic TVector<T> = class
  type
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
    procedure Append(AValue: T);
    procedure LTrim(AValue: Integer);
    procedure RTrim(AValue: Integer);
    procedure Sort(Options: SortOptions = []; gcomp: PCardinal = nil; gswap: PCardinal = nil);
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

procedure TVector.Sort(Options: SortOptions; gcomp, gswap: PCardinal);
var
  i, gap, order, min, max, pos: Integer;
  g1, g2: Cardinal;
  Temp: T;
  needswap, swapped: Boolean;
begin
  g1 := 0; g2 := 0;
  gap := FCount;
  swapped := True;
  if soReversed in Options then order := 1 else order := -1;
  while (gap > 1) or swapped do begin
    gap := trunc(gap / 1.3);
    if gap < 1 then gap := 1;
    swapped := False;
    for i := 0 to FCount - 1 - gap do begin
      needswap := False;
      if soEliminateNA in Options then begin
        if FData[i + gap] = FDefault then Continue;
        if FData[i] = FDefault then needswap := True;
      end;
      if not needswap then begin
        Inc(g1);
        needswap := order * OnSort(FData[i], FData[i + gap]) < 0;
      end;
      if needswap then begin
        Inc(g2);
        Temp := FData[i];
        FData[i] := FData[i + gap];
        FData[i + gap] := Temp;
        swapped := True;
      end;
    end;
  end;
  if gcomp <> nil then gcomp^ := g1;
  if gswap <> nil then gswap^ := g2;
  if soEliminateNA in Options then begin
     min := 0;
     max := FCount - 1;
     if FData[max] <> FDefault then Exit;
     if FData[min] = FDefault then begin
       FCount := 0;
       Exit;
     end;
     while max > min do begin
       pos := (min + max) div 2;
       if FData[pos] = FDefault then max := pos - 1 else min := pos + 1;
     end;
     if FData[max] = FDefault then FCount := max else FCount := max + 1;
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

procedure TVector.Append(AValue: T);
begin
  Item[FCount] := AValue;
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

