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
    lv: TListView;
    Memo1: TMemo;
    od: TOpenDialog;
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
    procedure lvData(Sender: TObject; Item: TListItem);
    procedure pbPaint(Sender: TObject);
  private
    ds: TDataSheet;
    procedure ReloadDatasheet;
  public
    procedure Log(msg: string);
  end;

var
  Form1: TForm1;
  c: TCanvas;
implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
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

procedure TForm1.lvData(Sender: TObject; Item: TListItem);
var
  i, c: Integer;
begin
  i := Item.Index;
  Item.Caption := ds[i].Header;
  for c := 1 to ds.Cols do Item.SubItems.Add(Format('%0.2f', [ds[i][c]]));
end;

procedure TForm1.pbPaint(Sender: TObject);
begin
  ds.Visualize(pb.Canvas, ctBase,
    '{"color": "EEEEEE", "border_color": "FF0000", "border_style": ".."}');
end;

procedure TForm1.ReloadDatasheet;
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
end;

procedure TForm1.Log(msg: string);
begin
  msg := Format('[%s] %s', [FormatDateTime('yyy-mm-dd hh:nn:ss', Now), msg]);
  Memo1.Lines.Add(msg);
end;

end.

