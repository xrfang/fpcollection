unit asycdo;
{$mode objfpc}{$H+}
interface

uses Classes {, SysUtils};

type
  TAsyncDo = class
  private type
    Worker = class(TThread)

    end;
  private
    NumWorker: Integer;
    wks: array of Worker;

  public
    constructor Create(workers: Integer; stack: Integer = DefaultStackSize);
    destructor Destroy; override;
  end;

implementation
constructor TAsyncDo.Create(workers: Integer; stack: Integer);
var
  i: Integer;
begin
  NumWorker := workers;
  SetLength(wks, workers);
  for i := 0 to workers - 1 do begin
    wks[i] := Worker.Create(False, stack);
  end;
end;

destructor TAsyncDo.Destroy;
var
  i: Integer;
begin
  for i := 0 to NumWorker - 1 do wks[i].Free;
end;

end.

