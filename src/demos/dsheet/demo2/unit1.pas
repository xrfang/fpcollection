unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, dsheet;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    leScatX: TLabeledEdit;
    leScatY: TLabeledEdit;
    lv: TListView;
    Memo1: TMemo;
    od: TOpenDialog;
    Panel2: TPanel;
    pb: TPaintBox;
    Panel1: TPanel;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvData(Sender: TObject; Item: TListItem);
    procedure pbMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pbPaint(Sender: TObject);
  private
    ds: TDataSheet;
    CrossHair: Boolean;
    procedure ReloadDatasheet;
  public
    procedure Log(msg: string);
  end;

var
  Form1: TForm1;
  c: TCanvas;
implementation
uses LCLType;
{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  ds := TDataSheet.Create;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  sl: TStringList;
begin
  if od.Execute then begin
    sl := TStringList.Create;
    try
      sl.LoadFromFile(od.FileName);
      ds.Import(sl, hBoth);
      ReloadDatasheet;
      pc.ActivePageIndex := 1;
      ds.Save(od.FileName + '.gz');
    finally
      sl.Free;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if od.Execute then begin
    ds.Load(od.FileName);
    ReloadDatasheet;
    pc.ActivePageIndex := 1;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ds.Free;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    CrossHair := False;
    pb.Cursor := crDefault;
    ds.DrawCursor(pb.Canvas, -1, -1);
  end;
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
  vx: Integer;
  vy: Real;
begin
  if CrossHair then begin
    ds.DrawCursor(pb.Canvas, -1, -1);
    ds.DrawCursor(pb.Canvas, X, Y);
    ds.MapS(pb.Canvas, X, Y, vx, vy);
    leScatX.Text := '';
    if vx >= 0 then leScatX.Text := Format('%s', [ds[vx].Header]);
    leScatY.Text := Format('%0.2f', [vy]);
  end;
end;

procedure TForm1.pbPaint(Sender: TObject);
begin
  ds.Visualize(pb.Canvas, ctBase, '{"color": "EEEEEE,aaaaaa", "style": "-.."}');
  ds.Visualize(pb.Canvas, ctScat, '{"color": "#FF0000", "data": "1,4"}');
  if not ds.SyncView(pb.Canvas, [5]) then Exit;
  ds.Visualize(pb.Canvas, ctBars, '{"data": "5", "colors": "6"}');
  if not ds.SyncView(pb.Canvas, [1, 2, 3, 4]) then Exit;
  ds.Visualize(pb.Canvas, ctOHLC, '{"color": "#0000ff,#ff00ff"}');
  ds.Visualize(pb.Canvas, ctLine, '{"data": "4", "color": "#0000FF", "style": "*-"}');
end;

procedure TForm1.ReloadDatasheet;
var
  i: Integer;
begin
  for i := 0 to ds.Rows - 1 do begin
    ds[i][5] := ds[i][4] - ds[i][1];
    if ds[i][5] > 0 then ds[i][6] := 0
    else                 ds[i][6] := $EEEEEE;
  end;
  ds.Headers[5] := 'diff';
  lv.Clear;
  lv.Columns.Clear;
  for i := 0 to ds.Cols do with lv.Columns.Add do begin
    Caption := ds.Headers[i];
    AutoSize := True;
  end;
  lv.Items.Count := ds.Rows;
  ds.Anchor := -1;
end;

procedure TForm1.Log(msg: string);
begin
  msg := Format('[%s] %s', [FormatDateTime('yyy-mm-dd hh:nn:ss', Now), msg]);
  Memo1.Lines.Add(msg);
end;

end.
