unit cache;
{$mode objfpc}{$H+}{$INLINE on}
interface
uses crc;

type
  TCache = class
  private
    N: PtrUInt;
    FWidth: Byte;
    FSize: LongWord;
    FBuckets: array of Pointer;
    function BucketOf(key: Pointer; keylen: PtrUInt): LongWord;
  public
    property Count: PtrUInt read N;
    property Size: LongWord read FSize;
    constructor Create(Width: Byte);
    destructor Destroy; override;
    function Get(key: Pointer; keylen: PtrUInt): Pointer;
    function Add(key: Pointer; keylen: PtrUInt; val: Pointer): Pointer;
    procedure Clear;
  end;

implementation

function TCache.BucketOf(key: Pointer; keylen: PtrUInt): LongWord; inline;
begin
  Result := crc32(0, key, keylen) and (FSize - 1);
end;

constructor TCache.Create(Width: Byte);
begin
  if      Width >= 32 then FWidth := 32
  else if Width <= 16 then FWidth := 16
  else                     FWidth := Width;
  FSize := 1 shl FWidth;
  SetLength(FBuckets, FSize);
  N := 0;
end;

destructor TCache.Destroy;
begin
  FBuckets := nil;
end;

function TCache.Get(key: Pointer; keylen: PtrUInt): Pointer; inline;
var
  idx: LongWord;
begin
  idx := BucketOf(key, keylen);
  Result := FBuckets[idx];
end;

function TCache.Add(key: Pointer; keylen: PtrUInt; val: Pointer): Pointer; inline;
var
  idx: LongWord;
begin
  idx := BucketOf(key, keylen);
  Result := FBuckets[idx];
  FBuckets[idx] := val;
  Inc(N);
end;

procedure TCache.Clear; inline;
begin
  N := 0;
  FillChar(FBuckets[0], FSize * SizeOf(Pointer), 0);
end;

end.

