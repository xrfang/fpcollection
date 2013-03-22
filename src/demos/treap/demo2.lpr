program demo2;
{$mode objfpc}{$H+}
uses Classes, sysutils, treap;
type
  PPerson = ^TPerson;
  TPerson = record
    name: string;
    age: Byte;
  end;
  { TRoller }
  TRoller = class(specialize TTreap<PPerson, Byte>)
  public
    function OrderByName(Key: TKey; Node: PNode): Integer;
    function OrderByAge(Key: TKey; Node: PNode): Integer;
    procedure Add(name: string; age: Integer);
  end;
{ TRoller }
function TRoller.OrderByName(Key: TKey; Node: PNode): Integer;
begin
  if Key^.name < Node^.Key^.name then
    Result := -1
  else if Key^.name > Node^.Key^.name then
    Result := 1
  else if Key^.age < Node^.Key^.age then
    Result := -1
  else if Key^.age > Node^.Key^.age then
    Result := 1
  else
    Result := 0;
end;

function TRoller.OrderByAge(Key: TKey; Node: PNode): Integer;
begin
  if Key^.age < Node^.Key^.age then
    Result := -1
  else if Key^.age > Node^.Key^.age then
    Result := 1
  else if Key^.name < Node^.Key^.name then
    Result := -1
  else if Key^.name > Node^.Key^.name then
    Result := 1
  else
    Result := 0;
end;

procedure TRoller.Add(name: string; age: Integer);
var
  p: PPerson;
begin
  New(p);
  p^.name := name;
  p^.age := age;
  Insert(p, 0);
end;

var
  n: TRoller.PNode;
  r1, r2: TRoller;
begin
  Randomize;
  r1 := TRoller.Create;
  r2 := TRoller.Create;
  with r1 do begin
    Comparator := @OrderByName;
    Add('Alice', Random(100));
    Add('Bob', Random(100));
    Add('Cindy', Random(100));
    Add('Douglas', Random(100));
    Add('Emma', Random(100));
    Add('Frank', Random(100));
    Add('Gina', Random(100));
    Add('Henry', Random(100));
    for n in r1 do begin
      WriteLn(Format('NAME: %s'#9'AGE: %d', [n^.Key^.name, n^.Key^.age]));
    end;
    WriteLn;
    r2.Comparator := @r2.OrderByAge;
    r2.Import(r1);
    for n in r2 do begin
      WriteLn(Format('NAME: %s'#9'AGE: %d', [n^.Key^.name, n^.Key^.age]));
    end;
    r2.Free;
    Free;
  end;
end.
