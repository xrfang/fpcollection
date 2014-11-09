unit cache;
{$mode objfpc}{$H+}{$INLINE on}
interface
uses crc;

type
  PCacheItem = ^TCacheItem;
  TCacheItem = record
    key: Pointer;
    val: Pointer;
  end;
  TCache = class
  private
    N: QWord;
    FWidth: Byte;
    FDepth: Byte;
    FSize: LongWord;
    FBuckets: array of Pointer;
    function BucketOf(key: Pointer; keylen: Cardinal): LongWord;
    function GetCapacity: QWord;
    function GetLoadFactor: Double;
  protected
    procedure Disposer({%H-}ptr: Pointer);
  public
    property Width: Byte read FWidth;
    property Depth: Byte read FDepth;
    property Capacity: QWord read GetCapacity;
    property LoadFactor: Double read GetLoadFactor;
    constructor Create(AWidth, ADepth: Byte);
    destructor Destroy; override;
    function Get(key: Pointer; keylen: Cardinal; out data: Pointer): Boolean;
    procedure Add(key: Pointer; keylen: Cardinal; val: Pointer);
    procedure Clear;
  end;

implementation

function TCache.BucketOf(key: Pointer; keylen: Cardinal): LongWord; inline;
begin
  Result := crc32(0, key, keylen) and (FSize - 1);
end;

function TCache.GetCapacity: QWord; inline;
begin
  Result := FSize * FDepth;
end;

function TCache.GetLoadFactor: Double; inline;
begin
  Result := N / FSize / FDepth;
end;

procedure TCache.Disposer(ptr: Pointer); inline;
begin
  (* empty *)
end;

constructor TCache.Create(AWidth, ADepth: Byte);
begin
  if      AWidth >= 32 then FWidth := 32
  else if AWidth <= 16 then FWidth := 16
  else                      FWidth := AWidth;
  if ADepth = 0 then FDepth := 1 else FDepth := ADepth;
  FSize := 1 shl FWidth;
  SetLength(FBuckets, FSize);
  N := 0;
end;

destructor TCache.Destroy;
begin
  Clear;
  FBuckets := nil;
end;

function TCache.Get(key: Pointer; keylen: Cardinal; out data: Pointer): Boolean;
  inline;
var
  idx: LongWord;
  buf: Pointer;
  i: Integer;
  ci: PCacheItem;
begin
  Result := False;
  data := nil;
  idx := BucketOf(key, keylen);
  buf := FBuckets[idx];
  if buf <> nil then for i := 0 to FDepth - 1 do begin
    ci := PCacheItem((buf + SizeOf(Pointer) * i)^);
    if ci = nil then Continue;
    if CompareByte(key^, ci^.key^, keylen) = 0 then begin
      data := ci^.val;
      Exit(True);
    end;
  end;
end;

procedure TCache.Add(key: Pointer; keylen: Cardinal; val: Pointer); inline;
var
  idx: LongWord;
  buf, slot: Pointer;
  ci : PCacheItem;
  i : Byte;
begin
  idx := BucketOf(key, keylen);
  buf := FBuckets[idx];
  if buf = nil then begin
    buf := AllocMem(SizeOf(Pointer) * FDepth);
    Pointer(buf^) := GetMem(SizeOf(TCacheItem));
    FBuckets[idx] := buf;
    ci := PCacheItem(buf^);
    Inc(N);
  end else begin
    ci := nil;
    for i := 0 to FDepth - 1 do begin
      slot := buf + SizeOf(Pointer) * i;
      if Pointer(slot^) = nil then begin
        Inc(N);
        Pointer(slot^) := GetMem(SizeOf(TCacheItem));
        ci := PCacheItem(slot^);
        Break;
      end;
    end;
    if ci = nil then begin
      ci := PCacheItem((buf + SizeOf(Pointer) * Random(FDepth))^);
      Freemem(ci^.key);
    end;
  end;
  ci^.key := AllocMem(keylen);
  Move(key^, ci^.key^, keylen);
  ci^.val := val;
end;

procedure TCache.Clear; inline;
var
  i, j: Integer;
  ci: PCacheItem;
begin
  N := 0;
  for i := 0 to FSize - 1 do if FBuckets[i] <> nil then begin
    for j := 0 to FDepth - 1 do begin
      ci := PCacheItem((FBuckets[i] + j * SizeOf(Pointer))^);
      if ci <> nil then begin
        Freemem(ci^.key);
        Disposer(ci^.val);
        Dispose(ci);
      end;
    end;
    FreeMemory(FBuckets[i]);
  end;
  FillChar(FBuckets[0], FSize * SizeOf(Pointer), 0);
end;

initialization
Randomize;
end.

