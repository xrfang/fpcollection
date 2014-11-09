program demo;
uses sysutils, cache;
const
  ITEM_COUNT = 5000000;
var
  h: TCache;
  p: PChar;
  v: Pointer;
  b: Boolean;
  t: TDateTime;
  i, j, hit: PtrUInt;
  key: array[0..4] of PtrUInt;
begin
  SetHeapTraceOutput('heap.trc');
  p := 'Hello';
  h := TCache.Create(20, 2);
  WriteLn('Width=', h.Width, ', Depth=', h.Depth, ', Capacity=', h.Capacity);
  b := h.Get(p, strlen(p), v);
  WriteLn(BoolToStr(b, True));
  h.Add(p, strlen(p), Pointer(123));
  WriteLn(BoolToStr(h.Get(p, strlen(p), v), True));
  WriteLn(QWord(v));
  h.Clear;
  b := h.Get(p, strlen(p), v);
  WriteLn(BoolToStr(b, True));
  t := Now;
  for i := 0 to ITEM_COUNT - 1 do begin
    for j := 0 to 4 do key[j] := $deadbeef + i + j;
    h.Add(@key, SizeOf(PtrUInt) * 5, Pointer($deadbeef + i));
  end;
  t := Now - t;
  if t > 0 then
    WriteLn('Write Speed (Item/Sec): ', (ITEM_COUNT/t/86400):0:0);
  WriteLn('Load Factor (%): ', (h.LoadFactor * 100):0:3);
  hit := 0;
  t := Now;
  for i := 0 to ITEM_COUNT - 1 do begin
    for j := 0 to 4 do key[j] := $deadbeef + i + j;
    if h.Get(@key, SizeOf(PtrUInt) * 5, v) then Inc(hit);
  end;
  t := Now - t;
  if t > 0 then
    WriteLn('Read Speed (Item/Sec): ', (ITEM_COUNT/t/86400):0:0);
  WriteLn('Hit Rate (%): ', (hit / ITEM_COUNT * 100):0:3);
  h.Free;
end.

