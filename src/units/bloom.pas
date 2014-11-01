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
    procedure GetHash(buf: Pointer; len: PtrUInt);
  public
    property Count: PtrUInt read N;
    property Size: LongWord read FSize;
    constructor Create(Width: Byte);
    destructor Destroy; override;
    function Contains(buf: Pointer; len: PtrUInt): Boolean;
    function FalsePositive: Double;
    function Capacity(fp: Double): PtrUInt;
    procedure Add(buf: Pointer; len: PtrUInt);
    procedure Clear;
  end;

implementation

procedure TBloomFilter.GetHash(buf: Pointer; len: PtrUInt);
begin

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
begin

end;

function TBloomFilter.FalsePositive: Double;
begin
  Result := Power((1 - exp(-K * N / (2 shl (FWidth - 1)))), K);
end;

function TBloomFilter.Capacity(fp: Double): PtrUInt;
begin
  Result := Trunc((2 shl (FWidth - 1)) * ln(0.6185) / ln(fp));
end;

procedure TBloomFilter.Add(buf: Pointer; len: PtrUInt);
begin

end;

procedure TBloomFilter.Clear; inline;
begin
  N := 0;
  FillQWord(FBits[0], FSize div 8, 0);
end;

end.
