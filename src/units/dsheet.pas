unit dsheet;
{$mode objfpc}{$H+}
interface

uses
  Classes, Graphics, fpjson;
type
  TDataSheet = class
  type
    ChartType = (ctBase, ctOHLC, ctLine, ctBars, ctScat);
    HeaderOpt = (hNone, hColOnly, hRowOnly, hBoth);
    Row = class
    private
      FOwner: TDataSheet;
      FData: array of Double;
      FDefault: Double;
      function GetData(Index: Integer): Double;
      procedure SetData(Index: Integer; AValue: Double);
    public
      Header: string;
      property Data[Index: Integer]: Double read GetData write SetData; default;
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
    FCurX, FCurY: Integer;
    function GetHeader(Index: Integer): string;
    function GetRow(Index: Integer): Row;
    function GetRowCount: Integer;
    procedure SetHeader(Index: Integer; AValue: string);
    procedure SetMagnifier(AValue: Integer);
    function cut(var str: string): string;
    function CSSColor(spec: string): TColor;
    function CSSColor(spec: LongWord): TColor;
    function PenStyle(ps: string): TPenStyle;
    procedure Range(c: Integer; pannable: Boolean; var Min, Max: Real);
    function HMap(ARect: TRect; p: Integer): Integer;
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
    property Data[Index: Integer]: Row read GetRow; default;
    property Cols: Integer read FCols;
    property Headers[Index: Integer]: string read GetHeader write SetHeader;
    property Rows: Integer read GetRowCount;
    constructor Create(ADefault: Double = 0);
    destructor Destroy; override;
    function Append: Row;
    function Insert(Index: Integer): Row;
    procedure Clear(Complete: Boolean = True);
    procedure Delete(Index: Integer);
    procedure Load(fn: string);
    procedure Save(fn: string; fmt: string = '%e');
    function Export(fmt: string = '%0.4f'): TStringList;
    procedure Import(src: TStringList; WithHeaders: HeaderOpt);
    function SyncView(ACanvas: TCanvas; series: array of Integer): Boolean;
    function SyncView(ARect: TRect; series: array of Integer): Boolean;
    procedure Visualize(ACanvas: TCanvas; AType: ChartType; opts: string = '';
      ARect: Pointer = nil);
    procedure Visualize(ACanvas: TCanvas; AType: ChartType; opts: TJSONObject;
      ARect: Pointer = nil);
    procedure DrawCursor(ACanvas: TCanvas; X, Y: Integer; ARect: Pointer = nil);
    procedure MapS(ACanvas: TCanvas; px, py: Integer; out X: Integer;
      out Y: Real; ARect: Pointer = nil);
    procedure MapXY(ACanvas: TCanvas; px, py: Integer; out X, Y: Real;
      ARect: Pointer = nil);
  end;

implementation
uses sysutils, math, zstream, jsonparser;

function TDataSheet.Row.GetData(Index: Integer): Double;
var
  i : Integer;
begin
  Result := FDefault;
  i := Index - 1; //Index from 1, but internally starting from 0
  if i in [0..Length(FData)-1] then Result := FData[i];
end;

procedure TDataSheet.Row.SetData(Index: Integer; AValue: Double);
var
  i, j, c: Integer;
begin
  i := Index - 1; //Index from 1, but internally starting from 0
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

function TDataSheet.GetRow(Index: Integer): Row;
begin
  Result := Row(FRows[round(Index)]);
end;

function TDataSheet.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TDataSheet.GetHeader(Index: Integer): string;
begin
  if Index < FHeaders.Count then Exit(FHeaders[Index]) else Exit('');
end;

procedure TDataSheet.SetHeader(Index: Integer; AValue: string);
var
  j, c: Integer;
begin
  c := FHeaders.Count;
  if Index >= c then for j := c to Index do FHeaders.Add('');
  FHeaders[Index] := AValue;
end;

function TDataSheet.CSSColor(spec: string): TColor;
begin
  case Length(spec) of
    7: spec[1] := '$';
    6: spec := '$' + spec;
    else Exit(clBlack);
  end;
  Result := CSSColor(StrToIntDef(spec, 0));
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
  if (Length(ps) > 0) and (ps[1] = '*') then ps := Copy(ps, 2, 3);
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

function TDataSheet.cut(var str: string): string;
var
  p: Integer;
begin
  p := Pos(',', str);
  if p = 0 then begin
    Result := str;
    str := '';
  end else begin
    Result := Copy(str, 1, p - 1);
    str := Copy(str, p + 1, Length(str));
  end;
end;

procedure TDataSheet.Range(c: Integer; pannable: Boolean; var Min, Max: Real);
var
  i, ap, sp: Integer;
  v: Real;
begin
  if pannable then begin
    ap := FAnchor;
    sp := FAnchor - FSpan + 1;
  end else begin
    ap := Rows - 1;
    sp := 0;
  end;
  for i := ap downto sp do begin
    if i < 0 then Break;
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
  if FAnchor >= Rows then FAnchor := Rows - 1;
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

function TDataSheet.VMap(ARect: TRect; Min, Max, p: Real): Integer;
begin
  with ARect do Exit(Top + round((Max - p) / (Max - Min) * (Bottom - Top)));
end;

procedure TDataSheet.DrawBase(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  bcolor: TColor;
  bwidth: Integer;
  clrs, bstyle: string;
begin
  clrs := opts.Get('color', '#FFFFFF,#000000');
  FBGColor := CSSColor(cut(clrs));
  bcolor := CSSColor(cut(clrs));
  bstyle := opts.Get('style', '-');
  bwidth := ifthen(bstyle = '=', 2, 1);
  with ACanvas do begin
    Brush.Color := FBGColor;
    FillRect(ARect);
    with Pen do begin
      Color := bcolor;
      Width := bwidth;
      Style := PenStyle(bstyle);
      Rectangle(ARect);
    end;
  end;
end;

procedure TDataSheet.DrawOHLC(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  clr, clr_0, clr_1, clr_2: TColor;
  oc, hc, lc, cc, i, x, first: Integer;
  o, h, l, c: Integer;
  s: string;
begin
  if Cols < 4 then Exit;
  s := opts.Get('color', '#008000,#FF0000');
  clr_0 := CSSColor(cut(s));
  clr_1 := CSSColor(cut(s));
  clr_2 := CSSColor(cut(s));
  s := opts.Get('data', '1,2,3,4');
  oc := StrToIntDef(cut(s), 1);
  hc := StrToIntDef(cut(s), 2);
  lc := StrToIntDef(cut(s), 3);
  cc := StrToIntDef(cut(s), 4);;
  first := FAnchor - FSpan + 1;
  if first < 0 then first := 0;
  with ACanvas do begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    for i := first to FAnchor do begin
      x := HMap(ARect, i - first);
      o := VMap(ARect, FMinY, FMaxY, Data[i][oc]);
      h := VMap(ARect, FMinY, FMaxY, Data[i][hc]);
      l := VMap(ARect, FMinY, FMaxY, Data[i][lc]);
      c := VMap(ARect, FMinY, FMaxY, Data[i][cc]);
      case Sign(Data[i][cc] - Data[i][oc]) of
        1: clr := clr_1;
        -1: clr := clr_0;
        else clr := clr_2;
      end;
      Pen.Color := clr;
      MoveTo(x, h);
      LineTo(x, l);
      if FMagnifier >= 5 then begin
        Brush.Color := ifthen(clr = clr_1, FBGColor, clr);
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
  w, i, x, c, d, first: Integer;
  style: string;
  procedure DrawDataPoint(x, y: Integer);
  var
    r: Integer;
    ps: TPenStyle;
  begin
    if style[1] <> '*' then Exit;
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
  c := StrToIntDef(opts.Get('data', '1'), 1);
  color := CSSColor(opts.Get('color', '#000000'));
  style := opts.Get('style', '*-');
  w := ifthen(style = '=', 2, 1);
  if w > FMagnifier div 2 then w := FMagnifier div 2;
  first := FAnchor - FSpan + 1;
  if first < 0 then first := 0;
  with ACanvas do begin
    Pen.Style := PenStyle(style);
    Pen.Color := color;
    Pen.Width := w;
    x := HMap(ARect, 0);
    d := VMap(ARect, FMinY, FMaxY, Data[first][c]);
    MoveTo(x, d);
    DrawDataPoint(x, d);
    for i := first to FAnchor do begin
      x := HMap(ARect, i - first);
      d := VMap(ARect, FMinY, FMaxY, Data[i][c]);
      LineTo(x, d);
      DrawDataPoint(x, d);
    end;
  end;
end;

procedure TDataSheet.DrawBars(ACanvas: TCanvas; ARect: TRect; opts: TJSONObject);
var
  clrs: array of TColor;
  s: string;
  dp, ds, cc, cl, w, i, x, d1, d2, first: Integer;
begin
  if Cols < 1 then Exit;
  s := opts.Get('data', '1');
  dp := StrToIntDef(cut(s), 1);
  ds := StrToIntDef(cut(s), 0);
  if (dp = 0) or (dp > Cols) then Exit;
  clrs := nil;
  s := opts.Get('color', '#000000');
  if (Length(s) > 0) and (s[1] = '@') then begin
    cc := StrToIntDef(Copy(s, 2, Length(s)), 0);
  end else cc := 0;
  if cc = 0 then repeat
    cl := Length(clrs);
    SetLength(clrs, cl + 1);
    clrs[cl] := CSSColor(cut(s));
  until s = '';
  if opts.Get('style', '=') = '-' then w := 0 else w := FMagnifier div 2;
  cl := Length(clrs);
  first := FAnchor - FSpan + 1;
  if first < 0 then first := 0;
  with ACanvas do begin
    Pen.Style := psSolid;
    Pen.Width := 1;
    Pen.Color := clMaroon;
    Brush.Color := clMaroon;
    for i := first to FAnchor do begin
      x := HMap(ARect, i - first);
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
  x, y, xc, yc, fp, lp, w, i: Integer;
  movable: Boolean;
  clr: TColor;
  s: string;
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
  if FAnchor >= Rows then FAnchor := Rows - 1;
  s := opts.Get('data', '1,2');
  xc := StrToIntDef(cut(s), 1);
  yc := StrToIntDef(cut(s), 2);
  if (xc < 1) or (xc > Cols) or (yc < 1) or (yc > Cols) then Exit;
  clr := CSSColor(opts.Get('color', '#000000'));
  case opts.Get('style', '*') of
    '.': w := 1;
    '@': w := 3;
    else w := 2;
  end;
  movable := StrToIntDef(opts.Get('full', '1'), 1) = 0;
  FMinX := 1e300; FMaxX := -1e300;
  FMinY := 1e300; FMaxY := -1e300;
  Range(xc, movable, FMinX, FMaxX);
  Range(yc, movable, FMinY, FMaxY);
  if movable then begin
    fp := FAnchor - FSpan + 1;
    lp := FAnchor;
  end else begin
    fp := 0;
    lp := Rows - 1;
  end;
  if fp < 0 then fp := 0;
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
  FCurX := -1;
  FCurY := -1;
end;

function TDataSheet.Insert(Index: Integer): Row;
begin
  Result := Row.Create(Self, FDefault);
  try
    FRows.Insert(Index, Result);
  except
    Result.Free;
    raise;
  end;
end;

procedure TDataSheet.Delete(Index: Integer);
begin
  Row(FRows[Index]).Free;
  FRows.Delete(Index);
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
  opts: string; ARect: Pointer);
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
  opts: TJSONObject; ARect: Pointer);
var
  r: TRect;
begin
  if ARect = nil then r := Rect(0, 0, ACanvas.Width - 1, ACanvas.Height - 1)
  else                r := TRect(ARect^);
  case AType of
    ctBase: DrawBase(ACanvas, r, opts);
    ctOHLC: DrawOHLC(ACanvas, r, opts);
    ctLine: DrawLine(ACanvas, r, opts);
    ctBars: DrawBars(ACanvas, r, opts);
    ctScat: DrawScat(ACanvas, r, opts);
  end;
end;

procedure TDataSheet.DrawCursor(ACanvas: TCanvas; X, Y: Integer; ARect: Pointer);
var
  r: TRect;
begin
  if ARect = nil then r := Rect(0, 0, ACanvas.Width, ACanvas.Height)
  else                r := TRect(ARect^);
  with ACanvas do begin
    Pen.Width := 1;
    Pen.Style := psSolid;
    Pen.Color := clWhite;
    Pen.Mode := pmXor;
    if X >= 0 then FCurX := X;
    if Y >= 0 then FCurY := Y;
    MoveTo(FCurX, r.Top); LineTo(FCurX, r.Bottom);
    MoveTo(r.Left, FCurY); LineTo(r.Right, FCurY);
    Pen.Mode := pmCopy;
  end;
end;

procedure TDataSheet.MapS(ACanvas: TCanvas; px, py: Integer; out X: Integer;
  out Y: Real; ARect: Pointer);
var
  first : Integer;
  r: TRect;
begin
  X := -1;
  if ARect = nil then r := Rect(0, 0, ACanvas.Width - 1, ACanvas.Height - 1)
  else                r := TRect(ARect^);
  if (r.Left = r.Right) or (r.Top = r.Bottom) then Exit;
  first := FAnchor - FSpan + 1;
  if first < 0 then first := 0;
  X := trunc((px - r.Left) / FMagnifier / 2) + first;
  if X >= Rows then X := -1;
  Y := FMinY + (FMaxY - FMinY) * (r.Bottom - py) / (r.Bottom - r.Top);
end;

procedure TDataSheet.MapXY(ACanvas: TCanvas; px, py: Integer; out X, Y: Real;
  ARect: Pointer);
var
  r: TRect;
begin
  if ARect = nil then r := Rect(0, 0, ACanvas.Width - 1, ACanvas.Height - 1)
  else                r := TRect(ARect^);
  if (r.Left = r.Right) or (r.Top = r.Bottom) then Exit;
  X := FMinX + (FMaxX - FMinX) * (px - r.Left) / (r.Right - r.Left);
  Y := FMinY + (FMaxY - FMinY) * (r.Bottom - py) / (r.Bottom - r.Top);
end;

destructor TDataSheet.Destroy;
begin
  Clear;
  FRows.Free;
  FHeaders.Free;
end;

end.

