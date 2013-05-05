program demo;
{$mode objfpc}
uses sysutils, treap;
type
  THistogram = class(specialize TTreap<Char, Integer>)
  protected
    procedure Disposer(Node: PNode); override;
  end;

var
  i: Integer;
  c: Char;
  r : Cardinal;
  tp : THistogram;
  n: THistogram.PNode;
  mk: Char;
  mv: Integer;

procedure THistogram.Disposer(Node: PNode);
begin
  WriteLn('Disposing: Key=', Node^.Key, ', Value=', Node^.Value);
end;

begin
  tp := THistogram.Create;
  WriteLn('Inserting A~Z to Treap, assigning random values...');
  for c := 'A' to 'Z' do begin
    if Random < 0.5 then tp.Insert(c, Random(100));
  end;
  WriteLn('Show ranks of all keys...');
  for c := 'A' to 'Z' do begin
    if tp.Find(c, r) = nil then begin
      Write(Format('%s>%d'#9, [c, r]));
    end else
      Write(Format('%s@%d'#9, [c, r]));
  end;
  WriteLn;
  Writeln('Get all items by rank...');
  for i := 1 to tp.Count do begin
    n := tp.Fetch(i);
    Write(Format('%d=%s'#9, [i, n^.Key]));
  end;
  WriteLn;
  Writeln('Get all items by enumerator...');
  for n in tp do Write(n^.Key + ' ');
  WriteLn;
  Writeln('Get all items reversed by enumerator...');
  for n in tp.Reversed do Write(n^.Key + ' ');
  WriteLn;
  WriteLn('# of items: ' + IntToStr(tp.Count));
  mv := -1;
  for n in tp do begin
    if n^.Value > mv then begin
      mv := n^.Value;
      mk := n^.Key;
    end;
    Write(Format('%s=%d'#9, [n^.Key, n^.Value]));
  end;
  WriteLn;
  WriteLn(Format('Max=%d at %s', [mv, mk]));
  WriteLn('X is ', tp.Value('X', -65536));
  WriteLn('x is ' + BoolToStr(tp.Delete('x'), 'Deleted', 'Not Found'));
  WriteLn('# of items: ' + IntToStr(tp.Count));
  WriteLn('X is ' + BoolToStr(tp.Delete('X'), 'Deleted', 'Not Found'));
  WriteLn('# of items: ' + IntToStr(tp.Count));
  WriteLn('A is ' + BoolToStr(tp.Insert('A', -1), 'Updated', 'Not Changed'));
  WriteLn('# of items: ' + IntToStr(tp.Count));
  WriteLn('a is ' + BoolToStr(tp.Insert('a', 65536), 'Updated', 'Not Changed'));
  WriteLn('# of items: ' + IntToStr(tp.Count));
  WriteLn('a is ' + BoolToStr(tp.Insert('a', 65536), 'Updated', 'Not Changed'));
  WriteLn('# of items: ' + IntToStr(tp.Count));
  mv := -1;
  for n in tp do begin
    if n^.Value > mv then begin
      mv := n^.Value;
      mk := n^.Key;
    end;
    Write(Format('%s=%d'#9, [n^.Key, n^.Value]));
  end;
  WriteLn;
  WriteLn(Format('Max=%d at %s', [mv, mk]));
  tp.Clear;
  WriteLn(Format('After Clear(), # of items=%d', [tp.Count]));
  tp.Free;
end.
