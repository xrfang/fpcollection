unit asyncdo;
{$mode objfpc}{$H+}
interface

uses {$IFDEF UNIX}cthreads,{$ENDIF} Classes, SysUtils;

type
  TAsyncDo = class
  private type
    Worker = class(TThread)
    private
      Signal: PRTLEvent;
      WorkLoad: PtrUInt;
      Doer: TAsyncDo;
      Data: PtrUInt;
      Stat: Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
    end;
  private
    wks: array of Worker;
  protected
    function IsWIP(Input, Data: PtrUInt): Boolean; virtual;
    procedure Task(input: PtrUInt); virtual;
  public
    constructor Create(workers: Integer);
    function Call(UserData: PtrUInt): Integer;
    function Finish(timeout: Integer = 0; poll: Word = 500): Boolean;
    destructor Destroy; override;
  end;

implementation

procedure TAsyncDo.Worker.Execute;
begin
  while not Terminated do begin
    RTLeventWaitFor(Signal);
    if Terminated then Exit;
    RTLeventResetEvent(Signal);
    Doer.Task(Data);
    Stat := 0;
  end;
end;

constructor TAsyncDo.Worker.Create;
begin
  inherited Create(False);
  Signal := RTLEventCreate;
end;

destructor TAsyncDo.Worker.Destroy;
begin
  RTLeventDestroy(Signal);
  inherited Destroy;
end;

function TAsyncDo.IsWIP(Input, Data: PtrUInt): Boolean;
begin
  Result := Input = Data;
end;

procedure TAsyncDo.Task(input: PtrUInt);
begin
  (* empty *)
end;

constructor TAsyncDo.Create(workers: Integer);
var
  i: Integer;
begin
  SetLength(wks, workers);
  for i := 0 to workers - 1 do begin
    wks[i] := Worker.Create;
    wks[i].Doer := Self;
    wks[i].Stat := 0;
    wks[i].Data := 0;
  end;
end;

function TAsyncDo.Call(UserData: PtrUInt): Integer;
var
  i: Integer;
begin
  for i := 0 to Length(wks) - 1 do if IsWIP(UserData, wks[i].Data) then Exit(1);
  for i := 0 to Length(wks) - 1 do if wks[i].Stat = 0 then begin
    wks[i].Stat := 1;
    wks[i].Data := UserData;
    RTLeventSetEvent(wks[i].Signal);
    Exit(0);
  end;
  Result := -1;
end;

function TAsyncDo.Finish(timeout: Integer; poll: Word): Boolean;
var
  i: Integer;
  working, countdown: Boolean;
begin
  if poll < 100 then poll := 100;
  if poll > 10000 then poll := 10000;
  countdown := timeout > 0;
  while True do begin
    working := False;
    for i := 0 to Length(wks) - 1 do if wks[i].Stat <> 0 then begin
      working := True;
      Break;
    end;
    if not working then Exit(True);
    if timeout < 0 then Exit(False);
    if countdown then timeout := timeout - poll;
    Sleep(poll);
  end;
end;

destructor TAsyncDo.Destroy;
var
  i: Integer;
begin
  for i := 0 to Length(wks) - 1 do wks[i].Free;
end;

end.

