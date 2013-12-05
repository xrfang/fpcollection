unit vector;
{$mode objfpc}{$H+}
interface
uses sysutils;
type
  generic TVector<T> = class
  type
    TSelfType = TVector;
    TSelfClass = class of TSelfType;
    DataType = array of T;
    SortOption = (soReversed, soEliminateNA);
    SortOptions = set of SortOption;
    TDisposer = procedure(Item: T) of object;
  private
    FCapacity: Cardinal;
    FCount: Integer;
    FData: DataType;
    FDefault: T;
    function GetItem(Index: Integer): T;
    procedure SetItem(Index: Integer; AValue: T);
    procedure AdjustCapacity;
  protected
    function OnSort(v1, v2: T): Integer; virtual;
  public
    property Capacity: Cardinal read FCapacity;
    property Count: Integer read FCount;
    property Item[Index: Integer]: T read GetItem write SetItem; default;
    property NA: T read FDefault;
    property Raw: DataType read FData;
    constructor Create(ADefault: T); virtual;
    destructor Destroy; override;
    procedure Push(AValue: T);
    function Pop: T;
    procedure Clear(Disp: TDisposer = nil);
    procedure Trim(ACount: Integer; Disp: TDisposer = nil);
    procedure Sort(Options: SortOptions = []);
  end;

implementation
function TVector.GetItem(Index: Integer): T;
begin
  if Index < FCount then Result := FData[Index] else Result := FDefault;
end;

procedure TVector.Trim(ACount: Integer; Disp: TDisposer);
var
  i: Integer;
begin
  if ACount <= 0 then Exit;
  if ACount >= FCount then Clear(Disp) else begin
    if Disp <> nil then for i := FCount - ACount to FCount - 1 do Disp(FData[i]);
    FCount := FCount - ACount;
    AdjustCapacity;
  end;
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

procedure TVector.SetItem(Index: Integer; AValue: T);
var
  c: Integer;
begin
  if Index >= FCount then begin
    c := FCount;
    FCount := Index + 1;
    AdjustCapacity;
    while c < Index do begin FData[c] := FDefault; Inc(c); end;
  end;
  FData[Index] := AValue;
end;

procedure TVector.AdjustCapacity;
var
  n: Cardinal;
begin
  if FCount = 0 then n := 0 else n := 1 shl round(ln(FCount) / ln(2) + 0.5);
  if n <> FCapacity then begin
    FCapacity := n;
    SetLength(FData, FCapacity);
  end;
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
  Clear;
end;

destructor TVector.Destroy;
begin
  FData := nil;
end;

procedure TVector.Push(AValue: T);
begin
  Item[FCount] := AValue;
end;

function TVector.Pop: T;
begin
  Dec(FCount);
  Result := FData[FCount];
  AdjustCapacity;
end;

procedure TVector.Clear(Disp: TDisposer);
var
  i: Integer;
begin
  if Disp <> nil then for i := 0 to FCount - 1 do Disp(FData[i]);
  FCount := 0;
  FCapacity := 0;
  FData := nil;
end;

end.

