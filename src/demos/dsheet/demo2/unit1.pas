unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  LResources, ExtCtrls, StdCtrls, dsheet, fpjson;

type
  TForm1 = class(TForm)
    cd: TColorDialog;
    cbStyle: TComboBox;
    cbSplitAxis: TCheckBox;
    cbScatFull: TCheckBox;
    cbNoB: TCheckBox;
    Label1: TLabel;
    lbLegend: TLabel;
    lv: TListView;
    pnBGColor: TPanel;
    pcopts: TPageControl;
    pb: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    pc: TPageControl;
    pnPingColor: TPanel;
    pnYinColor: TPanel;
    pnBorderColor: TPanel;
    pnYangColor: TPanel;
    rgOHLCStyle: TRadioGroup;
    rgScatStyle: TRadioGroup;
    rgBarStyle: TRadioGroup;
    sb: TScrollBar;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tsoScat: TTabSheet;
    tsoBars: TTabSheet;
    tsoLine: TTabSheet;
    tsoOHLC: TTabSheet;
    tsoBase: TTabSheet;
    tc: TTabControl;
    procedure cbNoBChange(Sender: TObject);
    procedure cbScatFullChange(Sender: TObject);
    procedure cbSplitAxisChange(Sender: TObject);
    procedure cbStyleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure lvData(Sender: TObject; Item: TListItem);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbPaint(Sender: TObject);
    procedure pnBGColorClick(Sender: TObject);
    procedure rgBarStyleSelectionChanged(Sender: TObject);
    procedure rgOHLCStyleSelectionChanged(Sender: TObject);
    procedure rgScatStyleSelectionChanged(Sender: TObject);
    procedure sbScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure tcChange(Sender: TObject);
  private
    CrossHair: Boolean;
    ds: TDataSheet;
    opts_base, opts_ohlc, opts_line, opts_bars, opts_scat: TJSONObject;
    function ColorCSS(c: TColor): string;
    procedure SyncScroll;
    procedure RefreshOpts;
    procedure InitListView;
    procedure LoadOHLCData;
    procedure LoadLineData;
    procedure LoadBarsData;
    procedure LoadScatData;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
uses LCLType;
{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  ds := TDataSheet.Create(0);
  opts_base := TJSONObject.Create;
  opts_ohlc := TJSONObject.Create;
  opts_line := TJSONObject.Create;
  opts_bars := TJSONObject.Create;
  opts_scat := TJSONObject.Create;
end;

procedure TForm1.cbStyleChange(Sender: TObject);
begin
  RefreshOpts;
end;

procedure TForm1.cbSplitAxisChange(Sender: TObject);
begin
  pb.Invalidate;
end;

procedure TForm1.cbScatFullChange(Sender: TObject);
begin
  pb.Invalidate;
end;

procedure TForm1.cbNoBChange(Sender: TObject);
begin
  RefreshOpts;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ds.Free;
  opts_base.Free;
  opts_ohlc.Free;
  opts_line.Free;
  opts_bars.Free;
  opts_scat.Free;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  re: Boolean;
begin
  re := True;
  case Key of
    VK_UP: ds.Magnifier := ds.Magnifier + 1;
    VK_DOWN: ds.Magnifier := ds.Magnifier - 1;
    VK_LEFT: ds.Anchor := ds.Anchor - 1;
    VK_RIGHT: ds.Anchor := ds.Anchor + 1;
    VK_HOME: ds.Anchor := 0;
    VK_END: ds.Anchor := -1;
    VK_ESCAPE: begin
      CrossHair := False;
      pb.Cursor := crDefault;
      ds.DrawCursor(pb.Canvas, -1, -1);
      lbLegend.Caption := '';
    end;
    else re := False;
  end;
  SyncScroll;
  if re then pb.Invalidate;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  tc.TabIndex := 1;
  tcChange(tc);
end;

procedure TForm1.lvData(Sender: TObject; Item: TListItem);
var
  i, c: Integer;
begin
  i := Item.Index;
  Item.Caption := ds[i].Header;
  for c := 1 to ds.Cols do Item.SubItems.Add(Format('%0.2f', [ds[i][c]]));
end;

procedure TForm1.pbMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  CrossHair := True;
  pb.Cursor := crNone;
end;

procedure TForm1.pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  sx: Integer;
  vx, vy: Real;
  r: TDataSheet.Row;
begin
  if CrossHair then begin
    ds.DrawCursor(pb.Canvas, -1, -1);
    ds.DrawCursor(pb.Canvas, X, Y);
    case tc.TabIndex of
      1: begin
        ds.MapS(pb.Canvas, X, Y, sx, vy);
        if sx >= 0 then begin
          r := ds[sx];
          lbLegend.Caption := Format('[%s] O=%0.2f; H=%0.2f; L=%0.2f; C=%0.2f' +
            #9'%0.0f', [r.Header, r[1], r[2], r[3], r[4], vy]);
        end else lbLegend.Caption := '';
      end;
      2: begin
        ds.MapS(pb.Canvas, X, Y, sx, vy);
        if sx >= 0 then begin
          r := ds[sx];
          lbLegend.Caption := Format('X=%0.4f; Y=%0.4f'#9'%0.2f', [r[1], r[2], vy]) ;
        end else lbLegend.Caption := '';
      end;
      3: begin
        ds.MapS(pb.Canvas, X, Y, sx, vy);
        if sx >= 0 then begin
          r := ds[sx];
          lbLegend.Caption := Format('X=%0.4f; Y=%0.4f'#9'%0.2f', [r[1], r[2], vy]) ;
        end else lbLegend.Caption := '';
      end;
      4: begin
        ds.MapXY(pb.Canvas, X, Y, vx, vy);
        lbLegend.Caption := Format('X=%0.4f'#9'Y=%0.4f', [vx, vy]);
      end;
    end;
  end;
end;

procedure TForm1.pbPaint(Sender: TObject);
var
  r: TRect;
begin
  r := Rect(1, 1, pb.Width - 1, pb.Height - 1);
  ds.Visualize(pb.Canvas, ctBase, opts_base, @r);
  case tc.TabIndex of
    1: begin
      case rgOHLCStyle.ItemIndex of
        0: opts_ohlc.Add('style', '-');
        1: opts_ohlc.Add('style', '+');
        2: opts_ohlc.Add('style', '#');
      end;
      ds.SyncView(r, [1, 2, 3, 4]);
      ds.Visualize(pb.Canvas, ctOHLC, opts_ohlc, @r);
    end;
    2: begin
      if cbSplitAxis.Checked then ds.SyncView(r, [1])
      else ds.SyncView(r, [1, 2]);
      opts_line.Add('data', '1');
      opts_line.Add('color', '#008000');
      opts_line.Add('style', '-..');
      ds.Visualize(pb.Canvas, ctLine, opts_line, @r);
      if cbSplitAxis.Checked then ds.SyncView(r, [2]);
      opts_line.Add('data', '2');
      opts_line.Add('color', '#FF0000');
      opts_line.Add('style', '*-');
      ds.Visualize(pb.Canvas, ctLine, opts_line, @r);
    end;
    3: begin
      ds.SyncView(r, [1,2]);
      case rgBarStyle.ItemIndex of
        0: opts_bars.Add('style', '-');
        1: opts_bars.Add('style', '+');
        2: opts_bars.Add('style', '#');
        3: opts_bars.Add('style', '+');
      end;
      if rgBarStyle.ItemIndex = 3 then begin
        opts_bars.Add('color', '#FF0000');
        opts_bars.Add('data', '1');
        ds.Visualize(pb.Canvas, ctBars, opts_bars, @r);
        opts_bars.Add('color', '#008000');
        opts_bars.Add('data', '1,2');
        ds.Visualize(pb.Canvas, ctBars, opts_bars, @r);
      end else begin
        opts_bars.Add('data', '1,2');
        opts_bars.Add('color', '@3');
        ds.Visualize(pb.Canvas, ctBars, opts_bars, @r);
      end;
    end;
    4: begin
      opts_scat.Add('color', '#FF0000');
      if cbScatFull.Checked then opts_scat.Add('full', '1')
      else                       opts_scat.Add('full', '0');
      case rgScatStyle.ItemIndex of
        0: opts_scat.Add('style', '.');
        1: opts_scat.Add('style', '*');
        2: opts_scat.Add('style', '@');
      end;
      ds.Visualize(pb.Canvas, ctScat, opts_scat, @r);
    end;
  end;
end;

procedure TForm1.pnBGColorClick(Sender: TObject);
begin
  with TPanel(Sender) do if cd.Execute then begin
    Color := cd.Color;
    Font.Color := InvertColor(cd.Color);
    RefreshOpts;
  end;
end;

procedure TForm1.rgBarStyleSelectionChanged(Sender: TObject);
begin
  Panel2.SetFocus;
  pb.Invalidate;
end;

procedure TForm1.rgOHLCStyleSelectionChanged(Sender: TObject);
begin
  Panel2.SetFocus;
  pb.Invalidate;
end;

procedure TForm1.rgScatStyleSelectionChanged(Sender: TObject);
begin
  Panel2.SetFocus;
  pb.Invalidate;
end;

procedure TForm1.sbScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  ds.Anchor := trunc(ds.Rows * sb.Position / sb.Max);
  pb.Invalidate;
end;

procedure TForm1.tcChange(Sender: TObject);
begin
  pcopts.ActivePageIndex := tc.TabIndex;
  case tc.TabIndex of
    1: LoadOHLCData;
    2: LoadLineData;
    3: LoadBarsData;
    4: LoadScatData;
  end;
  RefreshOpts;
end;

function TForm1.ColorCSS(c: TColor): string;
var
  rgb: LongInt;
begin
  rgb := ColorToRGB(c);
  Result := IntToHex(rgb and $FF, 2) +
            IntToHex((rgb and $FF00) shr 8, 2) +
            IntToHex((rgb and $FF0000) shr 16, 2);
end;

procedure TForm1.SyncScroll;
begin
  sb.Position := round(ds.Anchor / ds.Rows * 100);
end;

procedure TForm1.RefreshOpts;
  procedure RefreshBaseOpts;
  var
    s, cs: string;
  begin
    opts_base.Clear;
    cs := ColorCSS(pnBGColor.Color);
    if not cbNoB.Checked then cs += ',' + ColorCSS(pnBorderColor.Color);
    opts_base.Add('color', cs);
    s := cbStyle.Text;
    if      s = 'solid'        then s := '-'
    else if s = 'dash'         then s := '--'
    else if s = 'dash-dot'     then s := '-.'
    else if s = 'dash-dot-dot' then s := '-..'
    else if s = 'dot'          then s := '..'
    else if s = 'none'         then s := '';
    opts_base.Add('style', s);
  end;
  procedure RefreshOHLCOpts;
  begin
    opts_ohlc.Clear;
    opts_ohlc.Add('color', ColorCSS(pnYinColor.Color) + ',' +
                           ColorCSS(pnYangColor.Color) + ',' +
                           ColorCSS(pnPingColor.Color));
  end;
  procedure RefreshLineOpts;
  begin
    opts_line.Clear;
  end;
  procedure RefreshBarsOpts;
  begin
    opts_bars.Clear;
  end;
  procedure RefreshScatOpts;
  begin
    opts_scat.Clear;
  end;
begin
  RefreshBaseOpts;
  RefreshOHLCOpts;
  RefreshLineOpts;
  RefreshBarsOpts;
  RefreshScatOpts;
  pb.Invalidate;
end;

procedure TForm1.InitListView;
var
  i: Integer;
begin
  lv.Clear;
  lv.Columns.Clear;
  for i := 0 to ds.Cols do with lv.Columns.Add do begin
    Caption := ds.Headers[i];
    AutoSize := True;
  end;
  lv.Items.Count := ds.Rows;
  ds.Anchor := -1;
end;

procedure TForm1.LoadOHLCData;
var
  r: TLResource;
  sl: TStringList;
begin
  r := LazarusResources.Find('ohlc');
  sl := TStringList.Create;
  try
    sl.Text := r.Value;
    ds.Import(sl, hBoth);
    InitListView;
  finally
    sl.Free;
  end;
end;

procedure TForm1.LoadLineData;
var
  i: Integer;
  r: TDataSheet.Row;
begin
  ds.Clear;
  for i := 0 to 99 do begin
    r := ds.Append;
    r[1] := i;
    r[2] := i * i * sin(i / 50 * pi);
  end;
  ds.Headers[1] := 'x';
  ds.Headers[2] := 'x^2 * sin(x / 50 * pi)';
  InitListView;
end;

procedure TForm1.LoadBarsData;
var
  i: Integer;
  r: TDataSheet.Row;
begin
  ds.Clear;
  for i := 0 to 19 do begin
    r := ds.Append;
    r[1] := i * i;
    r[2] := i * i * sin(i / 20);
    if i < 11 then r[3] := 0 else r[3] := ColorToRGB(pnBGColor.Color);
  end;
  ds.Headers[1] := 'x^2';
  ds.Headers[2] := 'x^2 * sin(x / 20)';
  ds.Headers[3] := 'colors';
  InitListView;
end;

procedure TForm1.LoadScatData;
var
  i: Integer;
  r: TDataSheet.Row;
begin
  ds.Clear;
  Randomize;
  for i := 0 to 2000 do begin
    r := ds.Append;
    r[1] := Random;
    r[2] := ln(r[1] + 0.0001) * sin(r[1]);
  end;
  ds.Headers[1] := 'x (random)';
  ds.Headers[2] := 'ln(x + 0.0001) * sin(x)';
  InitListView;
end;

initialization
{$I ohlc.lrs}

end.

