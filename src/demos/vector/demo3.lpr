program demo3;
{$mode objfpc}{$H+}
uses
  vector;
type
  TIntStack = specialize TVector<Integer>;
var
  i: Integer;
  st: TIntStack;
begin
  st := TIntStack.Create(0);
  for i := 1 to 100 do begin
    st.Push(i);
    WriteLn('Pushed: ', i, ', count=', st.Count, ', cap=', st.Capacity);
  end;
  for i := 0 to 99 do
    WriteLn('Poped: ', st.Pop, ', count=', st.Count, ', cap=', st.Capacity);
  WriteLn('After pop 10 items, count=', st.Count);
  st.Free;
end.

