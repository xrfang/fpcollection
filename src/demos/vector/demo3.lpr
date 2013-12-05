program demo3;
{$mode objfpc}{$H+}
uses
  vector;
type
  TIntStack = class(specialize TVector<Integer>)
  public
    procedure Discard(Value: Integer);
  end;

var
  i: Integer;
  st: TIntStack;

procedure TIntStack.Discard(Value: Integer);
begin
  WriteLn('Discarding: ', Value);
end;

begin
  st := TIntStack.Create(0);
  for i := 1 to 100 do begin
    st.Push(i);
    WriteLn('Pushed: ', i, ', count=', st.Count, ', cap=', st.Capacity);
  end;
  for i := 0 to 99 do
    WriteLn('Poped: ', st.Pop, ', count=', st.Count, ', cap=', st.Capacity);
  WriteLn('After pop 10 items, count=', st.Count);
  WriteLn('Populate vector with 10 items...');
  for i := 1 to 10 do st.Push(i);
  WriteLn('Trim vector to 5 elements...');
  st.Trim(5, @st.Discard);
  Write('Remaining: ');
  for i := 0 to st.Count - 1 do Write(st[i], '  ');
  WriteLn;
  WriteLn('Clear vector...');
  st.Clear(@st.Discard);
  st.Free;
end.

