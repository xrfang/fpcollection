unit c32hash;
{$mode objfpc}{$H+}{$INLINE on}
interface
uses crc;

type
  TC32Hash = class
  private
    N: PtrUInt;
    FWidth: Byte;
    FSize: LongWord;
    FBuckets: array of Pointer;
    function BucketOf(key: Pointer; keylen: Cardinal): LongWord;
  public
    property Count: PtrUInt read N;
    property Size: LongWord read FSize;
    constructor Create(Width: Byte);
    destructor Destroy; override;
    function Get(key: Pointer; keylen: Cardinal): Pointer;
    function Add(key: Pointer; keylen: Cardinal; val: Pointer): Pointer;
    procedure Clear;
  end;

implementation

function TC32Hash.BucketOf(key: Pointer; keylen: Cardinal): LongWord;
begin
  Result := crc32(0, key, keylen) and (FSize - 1);
end;

constructor TC32Hash.Create(Width: Byte);
begin
  if      Width >= 32 then FWidth := 32
  else if Width <= 16 then FWidth := 16
  else                     FWidth := Width;
  FSize := 1 shl FWidth;
  SetLength(FBuckets, FSize);
  N := 0;
end;

destructor TC32Hash.Destroy;
begin
  FBuckets := nil;
end;

function TC32Hash.Get(key: Pointer; keylen: Cardinal): Pointer;
var
  idx: LongWord;
begin
  idx := BucketOf(key, keylen);
  Result := FBuckets[idx];
end;

function TC32Hash.Add(key: Pointer; keylen: Cardinal; val: Pointer): Pointer;
var
  idx: LongWord;
begin
  idx := BucketOf(key, keylen);
  Result := FBuckets[idx];
  FBuckets[idx] := val;
  Inc(N);
end;

procedure TC32Hash.Clear; inline;
begin
  N := 0;
  FillChar(FBuckets[0], FSize * SizeOf(Pointer), 0);
end;

end.

