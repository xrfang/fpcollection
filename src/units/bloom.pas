unit bloom;
{$mode objfpc}{$H+}{$INLINE on}
interface
uses math, md5;
const
  HASH_WIDTH = 128;
type
  TBloomFilter = class
  private
    K: Byte;
    N: PtrUInt;
    FWidth: Byte;
    FSize: LongWord;
    FBits: array of Byte;
    HS: array [0..7] of LongWord;
    procedure MD5Chop(md: TMD5Digest);
public
    property Count: PtrUInt read N;
    property Size: LongWord read FSize;
    constructor Create(Width: Byte);
    destructor Destroy; override;
    function Add(buf: Pointer; len: PtrUInt): Boolean;
    function Contains(buf: Pointer; len: PtrUInt): Boolean;
    function FalsePositive: Double;
    function Capacity(fp: Double): PtrUInt;
    procedure Clear;
  end;

implementation

procedure TBloomFilter.MD5Chop(md: TMD5Digest); inline;
var
  a, b: QWord;
  i, c: Integer;
  m: QWord;
  w: Byte;
begin
  w := FWidth;
  m := QWord(1) shl w - 1;
  a := PQWord(@md[0])^; b := PQWord(@md[8])^;
  c := 64; i := 0;
  while c >= w do begin
    HS[i] := a and m;
    a := a shr w;
    Dec(c, w);
    Inc(i);
  end;
  if c > 0 then begin
    HS[i] := a or ((b and (2 shl (w - c) - 1)) shl c);
    b := b shr (w - c);
    c := 64 - w + c;
    Inc(i);
  end;
  c := 64;
  while c >= w do begin
    HS[i] := b and m;
    b := b shr w;
    Dec(c, w);
    Inc(i);
  end;
end;

constructor TBloomFilter.Create(Width: Byte);
begin
  if      Width >= 32 then FWidth := 32
  else if Width <= 16 then FWidth := 16
  else                    FWidth := Width;
  FSize := 2 shl (FWidth - 4);
  SetLength(FBits, FSize);
  K := HASH_WIDTH div FWidth;
  Clear;
end;

destructor TBloomFilter.Destroy;
begin
  FBits := nil;
end;

function TBloomFilter.Contains(buf: Pointer; len: PtrUInt): Boolean;
var
  i, mask: Byte;
  cell: LongWord;
begin
  Result := True;
  MD5Chop(MD5Buffer(buf^, len));
  for i := 0 to K - 1 do begin
    cell := (HS[i] and $FFFFFFF8) shr 3;
    mask := 1 shl (HS[i] and 7);
    if FBits[cell] and mask = 0 then Exit(False);
  end;
end;

function TBloomFilter.FalsePositive: Double;
begin
  Result := Power((1 - exp(-K * N / (2 shl (FWidth - 1)))), K);
end;

function TBloomFilter.Capacity(fp: Double): PtrUInt;
begin
  Result := Trunc((2 shl (FWidth - 1)) * ln(0.6185) / ln(fp));
end;

function TBloomFilter.Add(buf: Pointer; len: PtrUInt): Boolean; inline;
var
  i, mask: Byte;
  cell: LongWord;
begin
  Result := True;
  MD5Chop(MD5Buffer(buf^, len));
  for i := 0 to K - 1 do begin
    cell := (HS[i] and $FFFFFFF8) shr 3;
    mask := 1 shl (HS[i] and 7);
    Result := Result and (FBits[cell] and mask <> 0);
    FBits[cell] := FBits[cell] or mask;
  end;
end;

procedure TBloomFilter.Clear; inline;
begin
  N := 0;
  FillChar(FBits[0], FSize, 0);
end;

end.

