unit datatable;
{$mode objfpc}{$H+}
interface
uses Classes;
type
  TDataTable = class
  type
    Filter = function(data, default: Double): Double;
    Row = class
    private
      FOwner: TDataTable;
      FData: array of Double;
      FDefault: Double;
      function GetData(Index: Variant): Variant;
      function GetRaw: Pointer;
      procedure SetData(Index: Variant; AValue: Variant);
    public
      Header: string;
      property Data[Index: Variant]: Variant read GetData write SetData; default;
      property Raw: Pointer read GetRaw;
      constructor Create(AOwner: TDataTable; ADefault: Double);
      destructor Destroy; override;
      function {%H-}Equals(ARow: Row; Strict: Boolean = False): Boolean;
      procedure Assign(src: Row; Full: Boolean = False; f: Filter = nil);
    end;
  private
    FCols: Integer;
    FHeaders : TStringList;
    FRows: TList;
    FDefault : Double;
    function GetHeader(Index: Real): string;
    function GetRow(Index: Real): Row;
    function GetRowCount: Integer;
    procedure SetHeader(Index: Real; AValue: string);
  public
    property Data[Index: Real]: Row read GetRow; default;
    property Cols: Integer read FCols;
    property Headers[Index: Real]: string read GetHeader write SetHeader;
    property Rows: Integer read GetRowCount;
    constructor Create(ADefault: Double = 0);
    constructor Create(src: TDataTable; f: Filter = nil);
    destructor Destroy; override;
    function Append: Row;
    function Insert(Index: Real): Row;
    procedure Clear(Complete: Boolean = True);
    procedure Delete(Index: Real);
    procedure LoadFromFile(fn: string);
    procedure LoadFromStream(s: TStream);
    procedure SaveToFile(fn: string);
    procedure SaveToStream(s: TStream);
  end;

implementation
uses sysutils, variants;

const
  HEADER_BUFSIZE = 1024;

function TDataTable.Row.GetRaw: Pointer;
begin
  Result := Pointer(FData);
end;

function TDataTable.Row.GetData(Index: Variant): Variant;
var
  idx: Integer;
begin
  if VarIsStr(Index) then
    idx := FOwner.FHeaders.IndexOf(Index)
  else
    idx := Index;
  if idx < 0 then raise Exception.Create('Invalid Index: ' + Index);
  if idx = 0 then Exit(Header); //0 means row header
  idx -= 1; //Index starting from 1, but internal data starting from 0
  Result := FDefault;
  if idx < Length(FData) then Result := FData[idx];
end;

procedure TDataTable.Row.SetData(Index: Variant; AValue: Variant);
var
  idx, j, c: Integer;
begin
  if VarIsStr(Index) then
    idx := FOwner.FHeaders.IndexOf(Index)
  else
    idx := Index;
  if idx < 0 then raise Exception.Create('Invalid Index: ' + Index);
  if idx = 0 then begin //0 means row header
    Header := AValue;
    Exit;
  end;
  idx -= 1; //Index starting from 1, but internal data starting from 0
  c := Length(FData);
  if idx >= c then begin
    if FOwner.FCols <= idx then FOwner.FCols := idx + 1;
    SetLength(FData, idx + 1);
    for j := c to idx - 1 do FData[j] := FDefault;
  end;
  FData[idx] := AValue;
end;

constructor TDataTable.Row.Create(AOwner: TDataTable; ADefault: Double);
begin
  FOwner := AOwner;
  FDefault := ADefault;
  Header := '';
  SetLength(FData, 0);
end;

destructor TDataTable.Row.Destroy;
begin
  SetLength(FData, 0);
end;

function TDataTable.Row.Equals(ARow: Row; Strict: Boolean): Boolean;
var
  i, c: Integer;
begin
  Result := False;
  if Strict and (Header <> ARow.Header) then Exit;
  c := Length(ARow.FData);
  if Length(FData) > c then c := Length(FData);
  for i := 1 to c do if Data[i] <> ARow[i] then Exit;
  Result := True;
end;

procedure TDataTable.Row.Assign(src: Row; Full: Boolean; f: Filter);
var
  i, c: Integer;
begin
  if Full then Header := src.Header;
  c := Length(src.FData);
  SetLength(FData, c);
  for i := 0 to c - 1 do begin
    if f = nil then
      FData[i] := src.FData[i]
    else
      FData[i] := f(src.FData[i], FDefault);
  end;
end;

{ TDataTable }

function TDataTable.GetRow(Index: Real): Row;
begin
  Result := Row(FRows[round(Index)]);
end;

function TDataTable.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TDataTable.GetHeader(Index: Real): string;
var
  i: Integer;
begin
  i := round(Index);
  if i < FHeaders.Count then
    Result := FHeaders[i]
  else
    Result := '';
end;

procedure TDataTable.SetHeader(Index: Real; AValue: string);
var
  i, j, c: Integer;
begin
  c := FHeaders.Count;
  i := round(Index);
  if i >= c then for j := c to i do FHeaders.Add('');
  FHeaders[i] := AValue;
end;

constructor TDataTable.Create(ADefault: Double);
begin
  FDefault := ADefault;
  FRows := TList.Create;
  FHeaders := TStringList.Create;
  FHeaders.Add(''); //default column header for row headers
end;

constructor TDataTable.Create(src: TDataTable; f: Filter);
var
  i, c: Integer;
begin
  Create(src.FDefault);
  FHeaders.Assign(src.FHeaders);
  for i := 0 to src.Rows - 1 do begin
    c := Length(src[i].FData);
    if c > FCols then FCols := c;
    Append.Assign(src[i], True, f);
  end;
end;

function TDataTable.Insert(Index: Real): Row;
begin
  Result := Row.Create(Self, FDefault);
  try
    FRows.Insert(round(Index), Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TDataTable.Delete(Index: Real);
var
  i: Integer;
begin
  i := round(Index);
  Row(FRows[i]).Free;
  FRows.Delete(i);
end;

function TDataTable.Append: Row;
begin
  Result := Row.Create(Self, FDefault);
  FRows.Add(Result);
end;

procedure TDataTable.Clear(Complete: Boolean);
var
  i : Integer;
begin
  for i := 0 to FRows.Count - 1 do Row(FRows[i]).Free;
  FRows.Clear;
  FCols := 0;
  if Complete then FHeaders.Clear;
end;

procedure TDataTable.LoadFromStream(s: TStream);
var
  buf: PChar;
  c: Cardinal;
  i, j: Integer;
  d: Double;
  r: Row;
  procedure sRead(Ptr: Pointer; Size: Cardinal);
  var
    cnt: Integer;
  begin
    cnt := s.Read(Ptr^, Size);
    if cnt <> Size then
      raise EStreamError.CreateFmt('Reading %d bytes, got %d', [Size, cnt]);
  end;
begin
  Clear(True);
  buf := GetMem(HEADER_BUFSIZE + 1);
  try
    sRead(@c, SizeOf(c));
    for i := 0 to c - 1 do begin
      sRead(@c, SizeOf(c));
      if c > HEADER_BUFSIZE then
        raise Exception.Create('Data do not conform to spec (header too long).');
      if c > 0 then begin
        sRead(buf, c);
        buf[c] := #0;
        FHeaders.Add(buf);
      end else
        FHeaders.Add('');
    end;
    sRead(@c, SizeOf(c));
    for i := 1 to c do begin
      r := Append;
      sRead(@c, SizeOf(c));
      if c > HEADER_BUFSIZE then
        raise Exception.Create('Data do not conform to spec (header too long).');
      if c > 0 then begin
        sRead(buf, c);
        buf[c] := #0;
        r.Header := buf;
      end;
      sRead(@c, SizeOf(c));
      for j := 1 to c do begin
        sRead(@d, SizeOf(d));
        r[j] := d;
      end;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure TDataTable.SaveToStream(s: TStream);
var
  buf: PChar;
  c: Cardinal;
  i, j: Integer;
  r: Row;
  d: Double;
  procedure sWrite(Ptr: Pointer; Size: Cardinal);
  var
    cnt: Integer;
  begin
    cnt := s.Write(Ptr^, Size);
    if cnt <> Size then
      raise EStreamError.CreateFmt('Writing %d bytes, %d written', [Size, cnt]);
  end;
begin
  buf := GetMem(HEADER_BUFSIZE + 1);
  try
    c := FHeaders.Count;
    sWrite(@c, SizeOf(c));
    for i := 0 to c - 1 do begin
      c := Length(FHeaders[i]);
      if c > HEADER_BUFSIZE then c := HEADER_BUFSIZE;
      sWrite(@c, SizeOf(c));
      if c > 0 then begin
        StrPLCopy(buf, FHeaders[i], c);
        sWrite(buf, c);
      end;
    end;
    c := FRows.Count;
    sWrite(@c, SizeOf(c));
    for i := 0 to c - 1 do begin
      r := Row(FRows[i]);
      c := Length(r.Header);
      if c > HEADER_BUFSIZE then c := HEADER_BUFSIZE;
      sWrite(@c, SizeOf(c));
      if c > 0 then begin
        StrPLCopy(buf, r.Header, c);
        sWrite(buf, c);
      end;
      c := Length(r.FData);
      sWrite(@c, SizeOf(c));
      for j := 0 to c - 1 do begin
        d := r.FData[j];
        sWrite(@d, SizeOf(d));
      end;
    end;
  finally
    FreeMem(buf);
  end;
end;

procedure TDataTable.LoadFromFile(fn: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(fn, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TDataTable.SaveToFile(fn: string);
var
  fs: TFileStream;
begin
  ForceDirectories(ExtractFilePath(fn));
  fs := TFileStream.Create(fn, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

destructor TDataTable.Destroy;
begin
  Clear;
  FRows.Free;
  FHeaders.Free;
end;

end.

