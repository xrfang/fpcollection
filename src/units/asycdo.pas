unit asycdo;
{$mode objfpc}{$H+}
interface

uses Classes {, SysUtils};

type
  TAsyncDo = class
  private type
    TaskHandler = procedure(input: PtrUInt; output: Pointer) of object;
    Worker = class(TThread)
    private
      Signal: PRTLEvent;
      WorkLoad: PtrUInt;
      Handler: TaskHandler;
      IPtr: PtrUInt;
      OPtr: Pointer;
    public
      procedure Execute; override;
      constructor Create(const StackSize: SizeUInt);
    end;
  private
    NumWorker: Integer;
    wks: array of Worker;
  protected
    procedure Task(input: PtrUInt; output: Pointer = nil); virtual;
  public
    constructor Create(workers: Integer; stack: Integer = DefaultStackSize);
    destructor Destroy; override;
  end;

implementation

procedure TAsyncDo.Worker.Execute;
begin
  while not Terminated do begin
    RTLeventWaitFor(Signal);
    if Terminated then Exit;
    RTLeventResetEvent(Signal);
    Handler
    if not IsPrime then Number := 0;
    Application.QueueAsyncCall(@AddPrime, PtrInt(Self));
  end;
end;

constructor TAsyncDo.Worker.Create(const StackSize: SizeUInt);
begin
  inherited Create(False, StackSize);
  RTLeventSetEvent(Signal);
end;

procedure TAsyncDo.Task(input: PtrUInt; output: Pointer);
begin
  (* empty *)
end;

constructor TAsyncDo.Create(workers: Integer; stack: Integer);
var
  i: Integer;
begin
  NumWorker := workers;
  SetLength(wks, workers);
  for i := 0 to workers - 1 do begin
    wks[i] := Worker.Create(stack);
    wks[i].Handler := @Task;
  end;
end;

destructor TAsyncDo.Destroy;
var
  i: Integer;
begin
  for i := 0 to NumWorker - 1 do wks[i].Free;
end;

end.

