unit asyncdo;
{$mode objfpc}{$H+}
interface

uses Classes, SysUtils;

type
  TAsyncDo = class
  private type
    WorkerP = procedure(input: Pointer);
    WorkerM = procedure(input: Pointer) of object;
    Worker = class(TThread)
    private
      Signal: PRTLEvent;
      WorkLoad: PtrUInt;
      Doer: TAsyncDo;
      Data: Pointer;
      Stat: Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
    end;
  private
    wks: array of Worker;
    HType: Integer;
    HandlerP: WorkerP;
    HandlerM: WorkerM;
    procedure InitWorkers(n: Integer);
  protected
    function IsWIP(Input, Data: Pointer): Boolean; virtual;
  public
    constructor Create(workers: Integer; handler: WorkerP);
    constructor Create(workers: Integer; handler: WorkerM);
    function Call(UserData: Pointer): Integer;
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
    with Doer do if HType = 0 then HandlerP(Data) else HandlerM(Data);
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

function TAsyncDo.IsWIP(Input, Data: Pointer): Boolean;
begin
  Result := Input = Data;
end;

constructor TAsyncDo.Create(workers: Integer; handler: WorkerP);
begin
  InitWorkers(workers);
  HType := 0;
  HandlerP := handler;
end;

constructor TAsyncDo.Create(workers: Integer; handler: WorkerM);
begin
  InitWorkers(workers);
  HType := 1;
  HandlerM := handler;
end;

procedure TAsyncDo.InitWorkers(n: Integer);
var
  i: Integer;
begin
  SetLength(wks, n);
  for i := 0 to n - 1 do begin
    wks[i] := Worker.Create;
    wks[i].Doer := Self;
    wks[i].Stat := 0;
    wks[i].Data := nil;
  end;
end;

function TAsyncDo.Call(UserData: Pointer): Integer;
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
  for i := 0 to Length(wks) - 1 do with wks[i] do begin
    Terminate;
    RTLEventSetEvent(Signal);
    Free;
  end;
end;

end.

