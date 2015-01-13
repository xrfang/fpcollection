unit primefnd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  TPrimeFinder = class(TThread)
  private
    function IsPrime: Boolean;
  public
    Number: Integer;
    Barrier: PRTLEvent;
    procedure Execute; override;
    procedure AddPrime(data: PtrInt);
  end;

implementation
uses Unit1;

function TPrimeFinder.IsPrime: Boolean;
var
  i: Integer;
begin
  for i := 2 to Number - 1 do if Number mod i = 0 then Exit(False);
  Result := True;
end;

procedure TPrimeFinder.Execute;
begin
  while not Terminated do begin
    RTLeventWaitFor(Barrier);
    if Terminated then Exit;
    RTLeventResetEvent(Barrier);
    if not IsPrime then Number := 0;
    Application.QueueAsyncCall(@AddPrime, PtrInt(Self));
  end;
end;

procedure TPrimeFinder.AddPrime(data: PtrInt);
var
  tr: TPrimeFinder;
begin
  tr := TPrimeFinder(data);
  if tr.Number > 0 then Inc(fmMain.cnt);
  tr.Number := fmMain.num;
  RTLeventSetEvent(tr.Barrier);
  Inc(fmMain.num);
end;

end.

