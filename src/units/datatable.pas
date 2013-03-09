unit datatable;

{$mode objfpc}{$H+}

interface

uses
  Classes;
type
  TDataTable = class
  type
    Filter = function(data, default: Double): Double;
    Row = class
    private
      FData: array of Double;
      FDefault: Double;
      function GetData(Index: Real): Double;
      procedure SetData(Index: Real; AValue: Double);
    public
      Header: string;
      property Data[Index: Real]: Double read GetData write SetData; default;
      constructor Create(ADefault: Double);
      destructor Destroy; override;
      function {%H-}Equals(ARow: Row; Strict: Boolean = False): Boolean;
      procedure Assign(src: Row; Full: Boolean = False; f: Filter = nil);
    end;
  private
    FCols: Integer;
    FHeaders : TStringList;
    FLastRow : Row;
    FRows: TList;
    FDefault : Double;
    function GetColCount: Integer;
    function GetHeader(Index: Real): string;
    function GetRow(Index: Real): Row;
    function GetRowCount: Integer;
    procedure SetHeader(Index: Real; AValue: string);
    procedure UpdateCols(r: Row);
  public
    property Data[Index: Real]: Row read GetRow; default;
    property Cols: Integer read GetColCount;
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
uses sysutils;

const
  HEADER_BUFSIZE = 1024;

{ TDataTable.TRow }

function TDataTable.Row.GetData(Index: Real): Double;
var
  i : Integer;
begin
  i := round(Index) - 1; //Index starting from 1, but internal data starting from 0
  Result := FDefault;
  if i < Length(FData) then Result := FData[i];
end;

procedure TDataTable.Row.SetData(Index: Real; AValue: Double);
var
  i, j, c: Integer;
begin
  i := round(Index) - 1; //Index starting from 1, but internal data starting from 0
  c := Length(FData);
  if i >= c then begin
    SetLength(FData, i + 1);
    for j := c to i - 1 do FData[j] := FDefault;
  end;
  FData[i] := AValue;
end;

constructor TDataTable.Row.Create(ADefault: Double);
begin
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

function TDataTable.GetColCount: Integer;
begin
  UpdateCols(nil);
  Result := FCols;
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

procedure TDataTable.UpdateCols(r: Row);
var
  c: Integer;
begin
  if FLastRow <> nil then begin
    c := Length(FLastRow.FData);
    if c > FCols then FCols := c;
  end;
  FLastRow := r;
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
  Result := Row.Create(FDefault);
  try
    FRows.Insert(round(Index), Result);
    UpdateCols(Result);
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
  Result := Row.Create(FDefault);
  FRows.Add(Result);
  UpdateCols(Result);
end;

procedure TDataTable.Clear(Complete: Boolean);
var
  i : Integer;
begin
  for i := 0 to FRows.Count - 1 do Row(FRows[i]).Free;
  FRows.Clear;
  FCols := 0;
  FLastRow := nil;
  if Complete then FHeaders.Clear;
end;

procedure TDataTable.LoadFromStream(s: TStream);
var
  buf: PChar;
  c: Cardinal;
  i, j: Integer;
  d: Double;
  r: Row;
begin
  Clear(True);
  buf := GetMem(HEADER_BUFSIZE);
  s.Read(c{%H-}, SizeOf(c));
  for i := 0 to c - 1 do begin
    s.Read(c, SizeOf(c));
    if c > 0 then begin
      s.Read(buf^, c);
      buf[c] := #0;
      FHeaders.Add(buf);
    end else
      FHeaders.Add('');
  end;
  s.Read(c, SizeOf(c));
  for i := 1 to c do begin
    r := Append;
    s.Read(c, SizeOf(c));
    if c > 0 then begin
      s.Read(buf^, c);
      buf[c] := #0;
      r.Header := buf;
    end;
    for j := 1 to FHeaders.Count - 1 do begin
      s.Read(d{%H-}, SizeOf(d));
      r[j] := d;
    end;
  end;
  FreeMem(buf);
end;

procedure TDataTable.SaveToStream(s: TStream);
var
  buf: PChar;
  c: Cardinal;
  i, j: Integer;
  r: Row;
  d: Double;
begin
  buf := GetMem(HEADER_BUFSIZE);
  c := Cols + 1;
  s.Write(c, SizeOf(c));
  for i := 0 to c - 1 do begin
    c := Length(Headers[i]);
    if c > HEADER_BUFSIZE then c := HEADER_BUFSIZE;
    s.Write(c, SizeOf(c));
    if c > 0 then begin
      StrPLCopy(buf, Headers[i], c);
      s.Write(buf^, c);
    end;
  end;
  c := FRows.Count;
  s.Write(c, SizeOf(c));
  for i := 0 to c - 1 do begin
    r := Row(FRows[i]);
    c := Length(r.Header);
    if c > HEADER_BUFSIZE then c := HEADER_BUFSIZE;
    s.Write(c, SizeOf(c));
    if c > 0 then begin
      StrPLCopy(buf, r.Header, c);
      s.Write(buf^, c);
    end;
    for j := 1 to Cols do begin
      d := r[j];
      s.Write(d, SizeOf(d));
    end;
  end;
  FreeMem(buf);
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

