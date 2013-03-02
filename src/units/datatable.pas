unit datatable;

{$mode objfpc}{$H+}

interface

uses
  Classes;
type
  TDataTable = class
  type
    DataViewer = function(data: Double): Double;
    Row = class
    private
      BypassViewer: Boolean;
      FDefault: Double;
      FData: array of Double;
      function GetCount: Integer;
      function GetData(Index: Integer): Double;
      procedure SetCount(AValue: Integer);
      procedure SetData(Index: Integer; AValue: Double);
    public
      Viewer: DataViewer;
      Header: string;
      property Count: Integer read GetCount write SetCount;
      property Data[Index: Integer]: Double read GetData write SetData; default;
      constructor Create(ADefault: Double);
      destructor Destroy; override;
      function Equals(ARow: Row; CompareHeader: Boolean = False): Boolean;
      procedure Assign(ARow: Row; IncludeHeader: Boolean = False);
    end;
  private
    FHeaders : TStringList;
    FRows: TList;
    FDefault : Double;
    function GetColCount: Integer;
    function GetHeader(Index: Integer): string;
    function GetRaw(Index: Integer): Row;
    function GetRow(Index: Integer): Row;
    function GetRowCount: Integer;
    procedure SetHeader(Index: Integer; AValue: string);
  public
    Viewer : DataViewer;
    property Rows: Integer read GetRowCount;
    property Cols: Integer read GetColCount;
    property Data[Index: Integer]: Row read GetRow; default;
    property Raw[Index: Integer]: Row read GetRaw;
    property Headers[Index: Integer]: string read GetHeader write SetHeader;
    constructor Create(ADefault: Double = 0);
    function Insert(Index: Integer): Row;
    procedure Delete(Index: Integer);
    function Append: Row;
    procedure Clear(IncludingHeaders: Boolean = True);
    procedure LoadFromStream(s: TStream);
    procedure SaveToStream(s: TStream);
    procedure LoadFromFile(fn: string);
    procedure SaveToFile(fn: string);
    destructor Destroy; override;
  end;
implementation
uses sysutils;

const
  HEADER_BUFSIZE = 1024;

{ TDataTable.TRow }

function TDataTable.Row.GetData(Index: Integer): Double;
begin
  Index -= 1; //Index starting from 1, but internal data starting from 0
  Result := FDefault;
  if Index < Length(FData) then begin
    Result := FData[Index];
    if BypassViewer or (Viewer = nil) then Exit;
    try
      Result := Viewer(Result);
    except
      Result := FDefault;
    end;
  end;
end;

function TDataTable.Row.GetCount: Integer;
begin
  Result := Length(FData);
end;

procedure TDataTable.Row.SetCount(AValue: Integer);
begin
  SetLength(FData, AValue);
end;

procedure TDataTable.Row.SetData(Index: Integer; AValue: Double);
var
  i, c: Integer;
begin
  Index -= 1; //Index starting from 1, but internal data starting from 0
  c := Length(FData);
  if Index >= c then begin
    SetLength(FData, Index + 1);
    for i := c to Index - 1 do FData[i] := FDefault;
  end;
  FData[Index] := AValue;
end;

constructor TDataTable.Row.Create(ADefault: Double);
begin
  BypassViewer := False;
  FDefault := ADefault;
  Header := '';
  Viewer := nil;
  SetLength(FData, 0);
end;

destructor TDataTable.Row.Destroy;
begin
  SetLength(FData, 0);
end;

function TDataTable.Row.Equals(ARow: Row; CompareHeader: Boolean): Boolean;
var
  i, c: Integer;
begin
  Result := False;
  if CompareHeader and (Header <> ARow.Header) then Exit;
  Result := True;
  BypassViewer := True;
  ARow.BypassViewer := True;
  c := Length(ARow.FData);
  if Length(FData) > c then c := Length(FData);
  for i := 1 to c do
    if Data[i] <> ARow[i] then begin
      Result := False;
      Break;
    end;
  BypassViewer := False;
  ARow.BypassViewer := False;
end;

procedure TDataTable.Row.Assign(ARow: Row; IncludeHeader: Boolean);
var
  i, c: Integer;
begin
  if IncludeHeader then Header := ARow.Header;
  c := Length(ARow.FData);
  SetLength(FData, c);
  for i := 0 to c - 1 do FData[i] := ARow.FData[i];
end;

{ TDataTable }

function TDataTable.GetRaw(Index: Integer): Row;
begin
  Result := Row(FRows[Index]);
  Result.Viewer := nil;
end;

function TDataTable.GetRow(Index: Integer): Row;
begin
  Result := Row(FRows[Index]);
  Result.Viewer := Viewer;
end;

function TDataTable.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TDataTable.GetHeader(Index: Integer): string;
begin
  if Index < FHeaders.Count then
    Result := FHeaders[Index]
  else
    Result := '';
end;

function TDataTable.GetColCount: Integer;
begin
  Result := FHeaders.Count - 1;
end;

procedure TDataTable.SetHeader(Index: Integer; AValue: string);
var
  i, c: Integer;
begin
  c := FHeaders.Count;
  if Index >= c then for i := c to Index do FHeaders.Add('');
  FHeaders[Index] := AValue;
end;

constructor TDataTable.Create(ADefault: Double);
begin
  Viewer := nil;
  FDefault := ADefault;
  FRows := TList.Create;
  FHeaders := TStringList.Create;
end;

function TDataTable.Insert(Index: Integer): Row;
begin
  Result := Row.Create(FDefault);
  try
    FRows.Insert(Index, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TDataTable.Delete(Index: Integer);
begin
  Row(FRows[Index]).Free;
  FRows.Delete(Index);
end;

function TDataTable.Append: Row;
begin
  Result := Row.Create(FDefault);
  FRows.Add(Result);
end;

procedure TDataTable.Clear(IncludingHeaders: Boolean);
var
  i : Integer;
begin
  for i := 0 to FRows.Count - 1 do Row(FRows[i]).Free;
  FRows.Clear;
  if IncludingHeaders then FHeaders.Clear;
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
      buf[c + 1] := #0;
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
      buf[c + 1] := #0;
      r.Header := buf;
    end;
    for j := 1 to FHeaders.Count do begin
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
  c := FHeaders.Count;
  s.Write(c, SizeOf(c));
  for i := 0 to c - 1 do begin
    c := Length(FHeaders[i]);
    if c > HEADER_BUFSIZE then c := HEADER_BUFSIZE;
    s.Write(c, SizeOf(c));
    if c > 0 then begin
      StrPLCopy(buf, FHeaders[i], c);
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
    for j := 1 to FHeaders.Count do begin
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

