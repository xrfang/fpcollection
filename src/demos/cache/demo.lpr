program demo;
uses sysutils, c32hash;
const
  ITEM_COUNT = 5000000;
var
  h: TC32Hash;
  p: PChar;
  b: Boolean;
  t: TDateTime;
  i, j, fpc: PtrUInt;
  key: array[0..4] of PtrUInt;
begin
  p := 'Hello';
  h := TC32Hash.Create(20);
  WriteLn('Size=', h.Size);
  b := h.Add(p, strlen(p), p) <> nil;
  WriteLn(BoolToStr(b, True));
  WriteLn(BoolToStr(h.Get(p, strlen(p)) <> nil, True));
  b := h.Add(p, strlen(p), p) <> nil;
  WriteLn(BoolToStr(b, True));
  h.Clear;
  fpc := 0;
  t := Now;
  for i := 0 to ITEM_COUNT - 1 do begin
    for j := 0 to 4 do key[j] := $deadbeef + i + j;
    if h.Add(@key, SizeOf(PtrUInt) * 5, Pointer(i)) <> nil then Inc(fpc);
  end;
  t := Now - t;
  WriteLn('Item Count: ', h.Count);
  WriteLn('Speed (Item/Sec): ', (ITEM_COUNT/t/86400):0:0);
  WriteLn('Collision (%): ', (fpc / ITEM_COUNT * 100):0:3);
  h.Free;
end.

