unit datatable;

{$mode objfpc}{$H+}

interface

uses
  Classes;
type

  { TDataTable }

  TDataTable = class
  type
    Row = class
      FDefault: Double;
      FHeader: string;
      FData: array of Double;
    private
      function GetData(Index: Integer): Double;
      procedure SetData(Index: Integer; AValue: Double);
    public
      property Data[Index: Integer]: Double read GetData write SetData; default;
      property Header: string read FHeader write FHeader;
      constructor Create(ADefault: Double);
      destructor Destroy; override;
    end;
  private
    FHeaders : TStringList;
    FRows: TList;
    FDefault : Double;
    function GetColCount: Integer;
    function GetHeader(Index: Integer): string;
    function GetRow(Index: Integer): Row;
    function GetRowCount: Integer;
    procedure SetHeader(Index: Integer; AValue: string);
  public
    property Rows: Integer read GetRowCount;
    property Cols: Integer read GetColCount;
    property Data[Index: Integer]: Row read GetRow; default;
    property Headers[Index: Integer]: string read GetHeader write SetHeader;
    constructor Create(ADefault: Double = 0);
    function Insert(Index: Integer): Row;
    procedure Delete(Index: Integer);
    function Append: Row;
    procedure Clear;
    destructor Destroy; override;
  end;
implementation

{ TDataTable.TRow }

function TDataTable.Row.GetData(Index: Integer): Double;
begin
  Index -= 1; //Index starting from 1, but internal data starting from 0
  if Index >= Length(FData) then
    Result := FDefault
  else
    Result := FData[Index];
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
  FDefault := ADefault;
  FHeader := '';
  SetLength(FData, 0);
end;

destructor TDataTable.Row.Destroy;
begin
  SetLength(FData, 0);
end;

{ TDataTable }
function TDataTable.GetRow(Index: Integer): Row;
begin
  if Index < FRows.Count then
    Result := Row(FRows[Index])
  else
    Result := nil;
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

procedure TDataTable.Clear;
var
  i : Integer;
begin
  for i := 0 to FRows.Count - 1 do Row(FRows[i]).Free;
  FRows.Clear;
end;

destructor TDataTable.Destroy;
begin
  Clear;
  FRows.Free;
  FHeaders.Free;
end;

end.

