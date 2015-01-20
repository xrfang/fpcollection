program demo;
{$mode objfpc}{$H+}
uses sysutils, vector, asyncdo, Classes, EpikTimer;

type
  TPrimeFinder = class
  private
    cs: TRTLCriticalSection;
    FPrimes: TIntegerVector;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CheckPrime(n: PtrUInt);
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

procedure TPrimeFinder.CheckPrime(n: PtrUInt);
var
  i: Integer;
begin
  for i := 2 to n - 1 do if n mod i = 0 then Exit;
  EnterCriticalsection(cs);
  FPrimes.Push(n);
  LeaveCriticalsection(cs);
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

procedure Task(input: PtrUInt);
begin
  WriteLn('Working on task #', input);
  Sleep(1000 * (Random(5) + 1));
  WriteLn('Finished taske #', input);
end;

var
  ad: TAsyncDo;
  i, n, c: Integer;
  pf: TPrimeFinder;
  et: TEpikTimer;
begin
  n := 500000;
  et := TEpikTimer.Create(nil);
  pf := TPrimeFinder.Create;
  WriteLn('Non-parallel calculation (benchmark)...');
  et.Start;
  for i := 3 to n do pf.CheckPrime(i);
  et.Stop;
  WriteLn('Time elapsed: ', et.Elapsed:0:3);
  pf.Report;
  pf.Save('primes0.txt');
  pf.Wipe;
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
  pf.Free;
  et.Free;
end.

