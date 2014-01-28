unit dsheet;
{$mode objfpc}{$H+}
interface

uses
  Classes, Graphics, fpjson;
type
  PRect = ^TRect;
  TDataSheet = class
  type
    ChartType = (ctBase, ctOHLC, ctLine, ctBars, ctScat);
    HeaderOpt = (hNone, hColOnly, hRowOnly, hBoth);
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
    FMagnifier: Integer;
    FAnchor: Integer;
    FSpan: Integer;
    FBGColor: TColor;
    FMinX, FMaxX: Real;
    FMinY, FMaxY: Real;
    function GetHeader(Index: Real): string;
    function GetRow(Index: Real): Row;
    function GetRowCount: Integer;
    procedure SetHeader(Index: Real; AValue: string);
    procedure SetMagnifier(AValue: Integer);
    function CSSColor(spec: string): TColor;
    function CSSColor(spec: LongWord): TColor;
    function PenStyle(ps: string): TPenStyle;
    procedure Range(c: Integer; pannable: Boolean; var Min, Max: Real);
    function HMap(ARect: TRect; p: Integer): Integer;
    function HMap(ARect: TRect; Min, Max, p: Real): Integer;
    function VMap(ARect: TRect; Min, Max, p: Real): Integer;
    procedure DrawBase(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
    procedure DrawOHLC(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
    procedure DrawLine(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
    procedure DrawBars(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
    procedure DrawScat(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
  public
    property Anchor: Integer read FAnchor write FAnchor;
    property Magnifier: Integer read FMagnifier write SetMagnifier;
    property Span: Integer read FSpan;
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
    procedure Save(fn: string; fmt: string = '%e');
    function Export(fmt: string = '%0.4f'): TStringList;
    procedure Import(src: TStringList; WithHeaders: HeaderOpt);
    function SyncView(ACanvas: TCanvas; series: array of Integer): Boolean;
    function SyncView(ARect: TRect; series: array of Integer): Boolean;
    procedure Visualize(ACanvas: TCanvas; AType: ChartType; opts: string = '';
      ARect: PRect = nil);
    procedure Visualize(ACanvas: TCanvas; AType: ChartType; opts: TJSONObject;
      ARect: PRect = nil);
  end;

implementation
uses sysutils, math, zstream, jsonparser;

function TDataSheet.Row.GetData(Index: Real): Double;
var
  i : Integer;
begin
  Result := FDefault;
  i := round(Index) - 1; //Index from 1, but internally starting from 0
  if i in [0..Length(FData)-1] then Result := FData[i];
end;

procedure TDataSheet.Row.SetData(Index: Real; AValue: Double);
var
  i, j, c: Integer;
begin
  i := round(Index) - 1; //Index from 1, but internally starting from 0
  if i < 0 then Exit;
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

function TDataSheet.CSSColor(spec: string): TColor;
var
  c: Integer;
  r, g, b: Byte;
begin
  c := Length(spec);
  if c = 7 then begin
    spec := Copy(spec, 2, 6);
    c := 6;
  end;
  if c = 6 then try
    r := StrToInt('$' + Copy(spec, 1, 2));
    g := StrToInt('$' + Copy(spec, 3, 2));
    b := StrToInt('$' + Copy(spec, 5, 2));
    Exit(RGBToColor(r, g, b));
  except end;
  Exit(clBlack);
end;

function TDataSheet.CSSColor(spec: LongWord): TColor;
var
  r, g, b: Byte;
begin
  r := spec and $FF0000 shr 16;
  g := spec and $00FF00 shr 8;
  b := spec and $0000FF;
  Result := RGBToColor(r, g, b);
end;

function TDataSheet.PenStyle(ps: string): TPenStyle;
begin
  if (ps = '-') or (ps = '=') then Exit(psSolid);
  if ps = '--' then Exit(psDash);
  if ps = '..' then Exit(psDot);
  if ps = '-.' then Exit(psDashDot);
  if ps = '-..' then Exit(psDashDotDot);
  Exit(psClear);
end;

procedure TDataSheet.SetMagnifier(AValue: Integer);
begin
  if (AValue <= 0) or (AValue > 50) or (FMagnifier = AValue) then Exit;
  FMagnifier := AValue;
end;

procedure TDataSheet.Range(c: Integer; pannable: Boolean; var Min, Max: Real);
var
  i, ap, sp: Integer;
  v: Real;
begin
  if pannable then begin
    ap := FAnchor;
    sp := FAnchor - FSpan;
  end else begin
    ap := Rows - 1;
    sp := 0;
  end;
  for i := ap downto sp do begin
    v := Data[i][c];
    if v > Max then Max := v;
    if v < Min then Min := v;
  end;
end;

function TDataSheet.SyncView(ARect: TRect; series: array of Integer): Boolean;
var
  s: Integer;
begin
  if Length(series) = 0 then Exit(False);
  FSpan := (ARect.Right - ARect.Left) div (FMagnifier * 2);
  if FSpan <= 0 then Exit(False);
  if FAnchor < 0 then FAnchor += Rows;
  if FAnchor < FSpan then FAnchor := FSpan;
  FMinY := 1e300; FMaxY := -1e300;
  for s := 0 to Length(series) - 1 do begin
    if series[s] > Cols then Exit(False);
    Range(series[s], True, FMinY, FMaxY);
  end;
  Exit(True);
end;

function TDataSheet.SyncView(ACanvas: TCanvas; series: array of Integer): Boolean;
var
  r: TRect;
begin
  with ACanvas do r := Rect(0, 0, Width, Height);
  Result := SyncView(r, series);
end;

function TDataSheet.HMap(ARect: TRect; p: Integer): Integer;
begin
  Result := ARect.Left + (2 * p + 1) * FMagnifier;
end;

function TDataSheet.HMap(ARect: TRect; Min, Max, p: Real): Integer;
begin
  with ARect do Exit(Left + round((Max - p) / (Max - Min) * (Right - Left)));
end;

function TDataSheet.VMap(ARect: TRect; Min, Max, p: Real): Integer;
begin
  with ARect do Exit(Top + round((Max - p) / (Max - Min) * (Bottom - Top)));
end;

procedure TDataSheet.DrawBase(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  border_color: TColor;
  border_width: Integer;
  border_style: TPenStyle;
begin
  FBGColor := CSSColor(opts.Get('color', '#FFFFFF'));
  border_color := CSSColor(opts.Get('border_color', '#FFFFFF'));
  border_style := PenStyle(opts.Get('border_style', '-'));
  border_width := opts.Get('border_width', 1);
  with ACanvas do begin
    Brush.Color := FBGColor;
    FillRect(ARect);
    if border_style <> psClear then with Pen do begin
      Color := border_color;
      Width := border_width;
      Style := border_style;
      Rectangle(ARect);
    end;
  end;
end;

procedure TDataSheet.DrawOHLC(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  clr: TColor;
  oc, hc, lc, cc, i, x: Integer;
  o, h, l, c: Integer;
  color_0, color_1, color_2: TColor;
  ja: TJSONArray;
begin
  if Cols < 4 then Exit;
  color_0 := CSSColor(opts.Get('color_0', '#008000'));
  color_1 := CSSColor(opts.Get('color_1', '#FF0000'));
  color_2 := CSSColor(opts.Get('color_2', '#000000'));
  oc := 1; hc := 2; lc := 3; cc := 4;
  ja := opts.Find('data', jtArray) as TJSONArray;
  if (ja <> nil) and (ja.Count = 4) then begin
    oc := ja[0].AsInteger; hc := ja[1].AsInteger;
    lc := ja[2].AsInteger; cc := ja[3].AsInteger;
  end;
  with ACanvas do begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    for i := FAnchor - FSpan + 1 to FAnchor do begin
      x := HMap(ARect, i - FAnchor + FSpan - 1);
      o := VMap(ARect, FMinY, FMaxY, Data[i][oc]);
      h := VMap(ARect, FMinY, FMaxY, Data[i][hc]);
      l := VMap(ARect, FMinY, FMaxY, Data[i][lc]);
      c := VMap(ARect, FMinY, FMaxY, Data[i][cc]);
      case Sign(Data[i][cc] - Data[i][oc]) of
        1: clr := color_1;
        -1: clr := color_0;
        else clr := color_2;
      end;
      Pen.Color := clr;
      MoveTo(x, h);
      LineTo(x, l);
      if FMagnifier >= 5 then begin
        Brush.Color := ifthen(clr = color_1, FBGColor, clr);
        if o = c then begin
          MoveTo(x - FMagnifier div 2, o);
          LineTo(x + FMagnifier div 2, c);
        end else begin
          Rectangle(x - FMagnifier div 2, o, x + FMagnifier div 2, c);
        end;
      end;
    end;
  end;
end;

procedure TDataSheet.DrawLine(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  color: TColor;
  w, i, x, c, d: Integer;
  style: string;
  node: Boolean;
  procedure DrawDataPoint(x, y: Integer);
  var
    r: Integer;
    ps: TPenStyle;
  begin
    if not node then Exit;
    r := w + 2;
    if r > FMagnifier div 2 then r := FMagnifier div 2;
    with ACanvas do begin
      ps := Pen.Style;
      Pen.Style := psSolid;
      Pen.Width := 1;
      EllipseC(x, y, r, r);
      Pen.Style := ps;
      Pen.Width := w;
    end;
  end;
begin
  if Cols < 1 then Exit;
  c := opts.Get('data', 1);
  color := CSSColor(opts.Get('color', '#000000'));
  style := opts.Get('style', '-');
  node := opts.Get('node', False);
  w := ifthen(style = '=', 2, 1);
  if w > FMagnifier div 2 then w := FMagnifier div 2;
  with ACanvas do begin
    Pen.Style := PenStyle(style);
    Pen.Color := color;
    Pen.Width := w;
    x := HMap(ARect, 0);
    d := VMap(ARect, FMinY, FMaxY, Data[FAnchor - FSpan + 1][c]);
    MoveTo(x, d);
    DrawDataPoint(x, d);
    for i := FAnchor - FSpan + 1 to FAnchor do begin
      x := HMap(ARect, i - FAnchor + FSpan - 1);
      d := VMap(ARect, FMinY, FMaxY, Data[i][c]);
      LineTo(x, d);
      DrawDataPoint(x, d);
    end;
  end;
end;

procedure TDataSheet.DrawBars(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  clrs: array of TColor;
  dp, ds, cc, cl, w, i, x, d1, d2: Integer;
  jd: TJSONData;
begin
  if Cols < 1 then Exit;
  jd := opts.Find('data');
  dp := 0; ds := 0;
  if jd = nil then
    dp := 1
  else if jd.JSONType = jtNumber then
    dp := jd.AsInteger
  else if jd.JSONType = jtArray then begin
    if jd.Count > 0 then dp := jd.Items[0].AsInteger;
    if jd.Count > 1 then ds := jd.Items[1].AsInteger;
  end;
  if (dp = 0) or (dp > Cols) then Exit;
  clrs := nil;
  jd := opts.Find('colors');
  if jd = nil then begin
    SetLength(clrs, 1);
    clrs[0] := clBlack;
  end else if jd.JSONType = jtString then begin
    SetLength(clrs, 1);
    clrs[0] := CSSColor(jd.AsString);
  end else if jd.JSONType = jtArray then begin
    SetLength(clrs, jd.Count);
    for i := 0 to jd.Count - 1 do clrs[i] := CSSColor(jd.Items[i].AsString);
  end else if jd.JSONType = jtNumber then begin
    cc := jd.AsInteger;
  end else Exit;
  w := opts.Get('width', FMagnifier) div 2;
  if w > FMagnifier div 2 then w := FMagnifier div 2;
  if w < 1 then w := 1;
  cl := Length(clrs);
  with ACanvas do begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := clMaroon;
    Brush.Color := clMaroon;
    for i := FAnchor - FSpan + 1 to FAnchor do begin
      x := HMap(ARect, i - FAnchor + FSpan - 1);
      d1 := VMap(ARect, FMinY, FMaxY, Data[i][dp]);
      if ds > 0 then d2 := VMap(ARect, FMinY, FMaxY, Data[i][ds])
      else           d2 := VMap(ARect, FMinY, FMaxY, 0);
      if cl > 0 then Brush.Color := clrs[i mod cl]
      else           Brush.Color := CSSColor(round(Data[i][cc]));
      if Brush.Color = FBGColor then Pen.Color := InvertColor(FBGColor)
      else                           Pen.Color := Brush.Color;
      if w = 0 then begin
        MoveTo(x, d1);
        LineTo(x, d2);
      end else Rectangle(x - w, d1, x + w, d2);
    end;
  end;
end;

procedure TDataSheet.DrawScat(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  ja: TJSONArray;
  x, y, xc, yc, fp, lp, w, i: Integer;
  pannable: Boolean;
  clr: TColor;
  function XMap(p: Real): Integer;
  begin
    with ARect do
      Exit(Left + round((p - FMinX) / (FMaxX - FMinX) * (Right - Left)));
  end;
  function YMap(p: Real): Integer;
  begin
    with ARect do
      Exit(Top + round((FMaxY - p) / (FMaxY - FMinY) * (Bottom - Top)));
  end;
begin
  if Cols < 2 then Exit;
  FSpan := (ARect.Right - ARect.Left) div (FMagnifier * 2);
  if FSpan <= 0 then Exit;
  if FAnchor < 0 then FAnchor += Rows;
  if FAnchor < FSpan then FAnchor := FSpan;
  ja := opts.Find('data', jtArray) as TJSONArray;
  xc := 1; yc := 2;
  if ja <> nil then begin
    if ja.Count > 0 then xc := ja[0].AsInteger;
    if ja.Count > 1 then yc := ja[1].AsInteger;
  end;
  if (xc < 1) or (xc > Cols) or (yc < 1) or (yc > Cols) then Exit;
  clr := CSSColor(opts.Get('color', '#000000'));
  w := opts.Get('width', 0);
  if (w < 1) or (w > FMagnifier) then w := FMagnifier;
  w := w div 2; if w = 0 then w := 1;
  pannable := opts.Get('pannable', False);
  FMinX := 1e300; FMaxX := -1e300;
  FMinY := 1e300; FMaxY := -1e300;
  Range(xc, pannable, FMinX, FMaxX);
  Range(yc, pannable, FMinY, FMaxY);
  if pannable then begin
    fp := FAnchor - FSpan + 1;
    lp := FAnchor;
  end else begin
    fp := 0;
    lp := Rows - 1;
  end;
  with ACanvas do begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := clr;
    for i := fp to lp do begin
      x := XMap(Data[i][xc]);
      y := YMap(Data[i][yc]);
      Rectangle(x - w, y - w, x + w, y + w)
    end;
  end;
end;

constructor TDataSheet.Create(ADefault: Double);
begin
  FDefault := ADefault;
  FRows := TList.Create;
  FHeaders := TStringList.Create;
  FHeaders.Delimiter := ',';
  FHeaders.StrictDelimiter := True;
  FHeaders.Add(''); //default column header for row headers
  FMagnifier := 5;
  FAnchor := -1;
  FSpan := 0;
  FBGColor := clWhite;
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

procedure TDataSheet.Import(src: TStringList; WithHeaders: HeaderOpt);
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

procedure TDataSheet.Visualize(ACanvas: TCanvas; AType: ChartType;
  opts: string; ARect: PRect);
var
  jo: TJSONObject;
begin
  with TJSONParser.Create(opts) do try
    jo := Parse as TJSONObject;
    if jo = nil then jo := TJSONObject.Create;
    Visualize(ACanvas, AType, jo, ARect);
  finally
    jo.Free;
    Free;
  end;
end;

procedure TDataSheet.Visualize(ACanvas: TCanvas; AType: ChartType;
  opts: TJSONObject; ARect: PRect);
var
  r: TRect;
begin
  if ARect = nil then r := Rect(0, 0, ACanvas.Width - 1, ACanvas.Height - 1)
  else                r := ARect^;
  case AType of
    ctBase: DrawBase(ACanvas, r, opts);
    ctOHLC: DrawOHLC(ACanvas, r, opts);
    ctLine: DrawLine(ACanvas, r, opts);
    ctBars: DrawBars(ACanvas, r, opts);
    ctScat: DrawScat(ACanvas, r, opts);
  end;
end;

destructor TDataSheet.Destroy;
begin
  Clear;
  FRows.Free;
  FHeaders.Free;
end;

end.

