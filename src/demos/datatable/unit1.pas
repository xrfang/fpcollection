unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ComCtrls, datatable;

type

  { TForm1 }

  TForm1 = class(TForm)
    lv: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    od: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvData(Sender: TObject; Item: TListItem);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  dt: TDataTable;

implementation
{$R *.lfm}

{ TForm1 }

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
var
  i: Integer;
begin
  if od.Execute then begin
    dt.LoadFromFile(od.FileName);
    Caption := Format('[%s] - %s', [od.FileName, Application.Title]);
    lv.Columns.Clear;
    with lv.Columns.Add do begin
      Caption := '#';
      AutoSize := True;
    end;
    for i := 0 to dt.Cols do begin
      with lv.Columns.Add do begin
        Caption := dt.Headers[i];
        AutoSize := True;
      end;
    end;
    lv.Columns.Add;
    lv.Items.Count := dt.Rows;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  dt := TDataTable.Create(-1e300);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  dt.Free;
end;

procedure TForm1.lvData(Sender: TObject; Item: TListItem);
var
  i: Integer;
begin
  with Item do begin
    Caption := IntToStr(Index);
    SubItems.Add(dt[Index].Header);
    for i := 1 to dt.Cols do SubItems.Add(Format('%f', [dt[Index][i]]));
  end;
end;

end.

