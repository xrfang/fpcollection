program demo;
{$mode objfpc}{$H+}
uses sysutils, vector;
type
  TIntVector = class(specialize TVector<Integer>)
  protected
    procedure OnUpdate(Index: Integer; OldValue: T; var NewValue: T); override;
  end;
procedure TIntVector.OnUpdate(Index: Integer; OldValue: T; var NewValue: T);
begin
  WriteLn('Updating #', Index, ': OLD=', OldValue, ', NEW=', NewValue);
  if NewValue mod 2 = 0 then begin
    NewValue := -NewValue;
    WriteLn('negating even value... ', NewValue);
  end;
end;

procedure Dump(v: TIntVector);
var
  i: Integer;
begin
  WriteLn('count=', v.Count);
  for i := 0 to v.Count - 1 do Write(v[i], #9);
  WriteLn;
end;

var
  iv, iv2 : TIntVector;
  i : Integer;
  ans: string;
  r: TIntVector.DataType;
begin
  iv := TIntVector.Create(-1);
  for i := 10 to 20 do iv[i] := i + 1;
  iv.Append(123);
  Dump(iv);
  Write('Try to adjust frozen vector? [y/N] ');
  ReadLn(ans);
  ans := Trim(ans);
  if (ans <> '') and (LowerCase(ans[1]) = 'y') then iv.Frozen := True;
  WriteLn('RTrim 1 element...');
  iv.RTrim(1);
  Dump(iv);
  WriteLn('LTrim 10 elements...');
  iv.LTrim(10);
  Dump(iv);
  r := iv.Raw;
  WriteLn('Raw content of iv:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
  iv[0] := 13;
  WriteLn('Raw content of iv after change:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
  WriteLn('LTrim -2 elements...');
  iv.LTrim(-2);
  Dump(iv);
  WriteLn('Sort iv...');
  iv.Sort;
  Dump(iv);
  WriteLn('Sort iv reversed, eliminating NA values...');
  iv.Sort([soReversed, soEliminateNA]);
  Dump(iv);
  iv2 := TIntVector(iv.Clone);
  iv.Free;
  WriteLn('r is still accessible after iv is Destroyed, its content is unchanged:');
  WriteLn('count=', Length(r));
  for i := 0 to Length(r) - 1 do Write(r[i], #9);
  WriteLn;
  WriteLn('Clone of iv:');
  Dump(iv2);
  iv2.Free;
end.

