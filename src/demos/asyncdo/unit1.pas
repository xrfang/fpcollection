unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, primefnd, EpikTimer;

type

  { TfmMain }

  TfmMain = class(TForm)
    Button1: TButton;
    edThreads: TEdit;
    Label1: TLabel;
    m: TMemo;
    Panel1: TPanel;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    max_cores, max_threads: Integer;
    wk: array of TPrimeFinder;
    et: TEpikTimer;
    spent: Double;
  public
    num, cnt: Integer;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.lfm}

procedure TfmMain.Button1Click(Sender: TObject);
begin
  Button1.Enabled := False;
  m.Lines.Clear;
  max_cores := StrToIntDef(edThreads.Text, 1);
  if max_cores < 1 then max_cores := 1;
  max_threads := 0;
  Timer2.Enabled := True;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  et := TEpikTimer.Create(Self);
end;

procedure TfmMain.Timer1Timer(Sender: TObject);
var
  i : Integer;
  el: Double;
  s : string;
begin
  Timer1.Enabled := False;
  el := et.Elapsed;
  if el - spent > 6 then begin
    if m.Lines.Count = 0 then begin
      m.Lines.Add('');
      s := '';
    end else s := m.Lines[m.Lines.Count - 1];
    if s = '' then s := IntToStr(cnt) else s += ',' + IntToStr(cnt);
    m.Lines[m.Lines.Count - 1] := s;
    spent := el;
  end;
  if el >= 60 then begin
    for i := 0 to Length(wk) - 1 do wk[i].Terminate;
    wk := nil;
    et.Stop;
    Timer2.Enabled := True;
    m.Lines.Add('');
  end else Timer1.Enabled := True;
end;

procedure TfmMain.Timer2Timer(Sender: TObject);
var
  i : Integer;
begin
  Timer2.Enabled := False;
  Inc(max_threads);
  if max_threads > max_cores then begin
    Button1.Enabled := True;
    Exit;
  end;
  cnt := 0;
  num := 2;
  SetLength(wk, max_threads);
  spent := 0;
  et.Clear;
  et.Start;
  for i := 0 to max_threads - 1 do begin
    wk[i] := TPrimeFinder.Create(False);
    wk[i].FreeOnTerminate := True;
    wk[i].Barrier := RTLEventCreate;
    wk[i].Number := num;
    Inc(num);
    RTLeventSetEvent(wk[i].Barrier);
  end;
  Timer1.Enabled := True;
end;

end.

