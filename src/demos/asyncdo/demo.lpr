program demo;
{$mode objfpc}{$H+}
uses sysutils, vector, asyncdo, Classes, EpikTimer;

type
  TRange = record
    min, max: Integer;
  end;
  TPrimeFinder = class
  private
    cs: TRTLCriticalSection;
    FPrimes: TIntegerVector;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckPrime(r: Pointer);
    procedure Report;
    procedure Save(fn: string);
    procedure Wipe;
  end;

constructor TPrimeFinder.Create;
begin
  FPrimes := TIntegerVector.Create(0);
  InitCriticalSection(cs);
end;

destructor TPrimeFinder.Destroy;
begin
  DoneCriticalsection(cs);
  FPrimes.Free;
end;

procedure TPrimeFinder.CheckPrime(r: Pointer);
var
  k, i: Integer;
  isp: Boolean;
  g: TRange;
begin
  g := TRange(Pointer(r)^);
  WriteLn('min=', g.min, ', max=', g.max);
  for k := g.min to g.max do begin
    isp := True;
    for i := 2 to k - 1 do if k mod i = 0 then begin
      isp := False;
      Break;
    end;
    if isp then begin
      EnterCriticalsection(cs);
      FPrimes.Push(k);
      LeaveCriticalsection(cs);
    end;
  end;
end;

procedure TPrimeFinder.Report;
var
  i, c, s: LongWord;
begin
  FPrimes.Sort;
  WriteLn('# of primes found: ', FPrimes.Count);
  c := FPrimes.Count - 1;
  WriteLn('Smallest prime: ', FPrimes[0]);
  WriteLn('Largest prime: ', FPrimes[c]);
  s := 0;
  for i := 0 to c do s += FPrimes[i];
  WriteLn('Sum of all primes: ', s);
end;

procedure TPrimeFinder.Save(fn: string);
var
  i: Integer;
  sl: TStrings;
begin
  FPrimes.Sort;
  sl := TStringList.Create;
  try
    for i := 0 to FPrimes.Count - 1 do sl.Add(IntToStr(FPrimes[i]));
    sl.SaveToFile(fn);
  finally
    sl.Free;
  end;
end;

procedure TPrimeFinder.Wipe;
begin
  FPrimes.Clear;
end;

var
  ad: TAsyncDo;
  i, n, c: Integer;
  pf: TPrimeFinder;
  et: TEpikTimer;
  r: TRange;
begin
  n := 500000;
  et := TEpikTimer.Create(nil);
  pf := TPrimeFinder.Create;
  WriteLn('Non-parallel calculation (benchmark)...');
  r.min := 3; r.max := n;
  et.Start;
  pf.CheckPrime(@r);
  et.Stop;
  WriteLn('Time elapsed: ', et.Elapsed:0:3);
  pf.Report;
  pf.Save('primes0.txt');
  pf.Wipe;
{
  c := 1;
  WriteLn('Parallel calcuation, threads: ', c);
  ad := TAsyncDo.Create(c, @pf.CheckPrime);
  et.Clear;
  et.Start;
  i := 3;
  while i <= n do if ad.Call(i) >= 0 then Inc(i) else Sleep(1);
  ad.Finish;
  et.Stop;
  WriteLn('Time elapsed: ', et.Elapsed:0:3);
  pf.Report;
  pf.Save('primes' + IntToStr(c) + '.txt');
  ad.Free;
}
  pf.Free;
  et.Free;
end.

