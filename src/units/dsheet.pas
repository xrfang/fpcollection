unit dsheet;
{$mode objfpc}{$H+}
interface

uses
  Classes;
type
  TDataSheet = class
  type
    HeaderOpts = (hNone, hColOnly, hRowOnly, hBoth);
    Row = class
    private
      FOwner: TDataSheet;
      FData: array of Double;
      FDefault: Double;
      function GetData(Index: Real): Double;
      procedure SetData(Index: Real; AValue: Double);
    public
      Header: string;
      property Data[Index: Real]: Double read GetData write SetData; default;
      constructor Create(AOwner: TDataSheet; ADefault: Double);
      destructor Destroy; override;
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
    destructor Destroy; override;
    function Append: Row;
    function Insert(Index: Real): Row;
    procedure Clear(Complete: Boolean = True);
    procedure Delete(Index: Real);
    procedure Load(fn: string);
    procedure Save(fn: string; fmt: string = '%0.4f');
    function Export(fmt: string = '%0.4f'): TStringList;
    procedure Import(src: TStringList; WithHeaders: HeaderOpts);
    procedure Visualize(ACanvas: TCanvas
  end;

implementation
uses sysutils, zstream;

function TDataSheet.Row.GetData(Index: Real): Double;
var
  i : Integer;
begin
  i := round(Index) - 1; //Index from 1, but internally starting from 0
  Result := FDefault;
  if i < Length(FData) then Result := FData[i];
end;

procedure TDataSheet.Row.SetData(Index: Real; AValue: Double);
var
  i, j, c: Integer;
begin
  i := round(Index) - 1; //Index from 1, but internally starting from 0
  c := Length(FData);
  if i >= c then begin
    if FOwner.FCols <= i then FOwner.FCols := i + 1;
    SetLength(FData, i + 1);
    for j := c to i - 1 do FData[j] := FDefault;
  end;
  FData[i] := AValue;
end;

constructor TDataSheet.Row.Create(AOwner: TDataSheet; ADefault: Double);
begin
  FOwner := AOwner;
  FDefault := ADefault;
  Header := '';
  SetLength(FData, 0);
end;

destructor TDataSheet.Row.Destroy;
begin
  SetLength(FData, 0);
end;

function TDataSheet.GetRow(Index: Real): Row;
begin
  Result := Row(FRows[round(Index)]);
end;

function TDataSheet.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TDataSheet.GetHeader(Index: Real): string;
var
  i: Integer;
begin
  i := round(Index);
  if i < FHeaders.Count then
    Result := FHeaders[i]
  else
    Result := '';
end;

procedure TDataSheet.SetHeader(Index: Real; AValue: string);
var
  i, j, c: Integer;
begin
  c := FHeaders.Count;
  i := round(Index);
  if i >= c then for j := c to i do FHeaders.Add('');
  FHeaders[i] := AValue;
end;

constructor TDataSheet.Create(ADefault: Double);
begin
  FDefault := ADefault;
  FRows := TList.Create;
  FHeaders := TStringList.Create;
  FHeaders.Delimiter := ',';
  FHeaders.StrictDelimiter := True;
  FHeaders.Add(''); //default column header for row headers
end;

function TDataSheet.Insert(Index: Real): Row;
begin
  Result := Row.Create(Self, FDefault);
  try
    FRows.Insert(round(Index), Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TDataSheet.Delete(Index: Real);
var
  i: Integer;
begin
  i := round(Index);
  Row(FRows[i]).Free;
  FRows.Delete(i);
end;

procedure TDataSheet.Load(fn: string);
var
  sl: TStringList;
  zs: TGZFileStream;
begin
  zs := TGZFileStream.create(fn, gzopenread);
  sl := TStringList.Create;
  try
    sl.LoadFromStream(zs);
    Import(sl, hBoth);
  finally
    zs.Free;
    sl.Free;
  end;
end;

procedure TDataSheet.Save(fn: string; fmt: string);
var
  zs: TGZFileStream;
  sl: TStringList;
begin
  if not ForceDirectories(ExtractFilePath(fn)) then
    raise Exception.CreateFmt('%s.Save: mkdir failed - %s', [ClassName, fn]);
  zs := TGZFileStream.create(fn, gzopenwrite);
  sl := Export(fmt);
  try
    sl.SaveToStream(zs);
  finally
    zs.Free;
    sl.Free;
  end;
end;

function TDataSheet.Append: Row;
begin
  Result := Row.Create(Self, FDefault);
  FRows.Add(Result);
end;

procedure TDataSheet.Clear(Complete: Boolean);
var
  i : Integer;
begin
  for i := 0 to FRows.Count - 1 do Row(FRows[i]).Free;
  FRows.Clear;
  FCols := 0;
  if Complete then FHeaders.Clear;
end;

function TDataSheet.Export(fmt: string): TStringList;
var
  i, j: Integer;
  sl: TStringList;
  val: Double;
begin
  Result := TStringList.Create;
  Result.Add(FHeaders.DelimitedText);
  sl := TStringList.Create;
  sl.Delimiter := ',';
  sl.StrictDelimiter := True;
  try
    for i := 0 to Rows - 1 do begin
      sl.Clear;
      sl.Add(Data[i].Header);
      for j := 1 to Cols do begin
        val := Data[i][j];
        sl.Add(Format(fmt, [val]));
      end;
      Result.Add(sl.DelimitedText);
    end;
  finally
    sl.Free;
  end;
end;

procedure TDataSheet.Import(src: TStringList; WithHeaders: HeaderOpts);
var
  i, j: Integer;
  sl: TStringList;
  r: Row;
begin
  Clear;
  if src.Count = 0 then Exit;
  if WithHeaders in [hColOnly, hBoth] then begin
    FHeaders.DelimitedText := src[0];
    if WithHeaders = hColOnly then FHeaders.Insert(0, '');
    i := 1;
  end else i := 0;
  sl := TStringList.Create;
  try
    sl.Delimiter := ',';
    sl.StrictDelimiter := True;
    while i < src.Count do begin
      sl.DelimitedText := src[i];
      j := 0;
      r := Append;
      if WithHeaders in [hRowOnly, hBoth] then begin
        r.Header := sl[0];
        while j < sl.Count - 1 do begin
          Inc(j);
          r[j] := StrToFloatDef(sl[j], FDefault);
        end;
      end else begin
        r.Header := '';
        while j < sl.Count do begin
          r[j+1] := StrToFloatDef(sl[j], FDefault);
          Inc(j);
        end;
      end;
      Inc(i);
    end;
  finally
    sl.Free;
  end;
end;

destructor TDataSheet.Destroy;
begin
  Clear;
  FRows.Free;
  FHeaders.Free;
end;

end.

