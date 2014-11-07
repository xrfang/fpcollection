unit cache;
{$mode objfpc}{$H+}{$INLINE on}
interface
uses crc;

type
  PCacheItem = ^TCacheItem;
  TCacheItem = record
    val: Pointer;
    key: Pointer;
    len: Cardinal;
  end;
  TCache = class
  private
    FWidth: Byte;
    FDepth: Byte;
    FSize: LongWord;
    FBuckets: array of Pointer;
    function BucketOf(key: Pointer; keylen: Cardinal): LongWord;
    function GetCapacity: QWord;
  protected
    procedure Disposer(ptr: Pointer);
  public
    property Width: Byte read FWidth;
    property Depth: Byte read FDepth;
    property Capacity: QWord read GetCapacity;
    constructor Create(AWidth, ADepth: Byte);
    destructor Destroy; override;
    function Get(key: Pointer; keylen: Cardinal): Pointer;
    procedure Add(key: Pointer; keylen: Cardinal; val: Pointer);
    procedure Clear;
  end;

implementation

function TCache.BucketOf(key: Pointer; keylen: Cardinal): LongWord;
begin
  Result := crc32(0, key, keylen) and (FSize - 1);
end;

function TCache.GetCapacity: QWord; inline;
begin
  Result := FSize * FDepth;
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
end;

destructor TCache.Destroy;
begin
  FBuckets := nil;
end;

function TCache.Get(key: Pointer; keylen: Cardinal): Pointer;
var
  idx: LongWord;
begin
  idx := BucketOf(key, keylen);
  Result := FBuckets[idx];
end;

procedure TCache.Add(key: Pointer; keylen: Cardinal; val: Pointer);
var
  idx: LongWord;
  buf: Pointer;
begin
  idx := BucketOf(key, keylen);
  buf := FBuckets[idx];
  if buf = nil then begin
    buf := GetMemory(SizeOf(Pointer) * FDepth);
    FBuckets[idx] := buf;   //<-- WIP
  end;
  FBuckets[idx] := val;
end;

procedure TCache.Clear; inline;
var
  i, j: Integer;
  ci: PCacheItem;
begin
  for i := 0 to FSize - 1 do if FBuckets[i] <> nil then begin
    for j := 0 to FDepth - 1 do begin
      ci := PCacheItem(FBuckets[i] + j * SizeOf(Pointer));
      Disposer(ci^.val);
      Dispose(ci);
    end;
    FreeMemory(FBuckets[i]);
  end;
  FillChar(FBuckets[0], FSize * SizeOf(Pointer), 0);
end;

end.

