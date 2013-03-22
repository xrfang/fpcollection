program demo;
{$mode objfpc}
uses sysutils, treap;
type

  { THistogram }

  THistogram = class(specialize TTreap<Char, Integer>)
  protected
    function DefaultTraverser(Key: Char; Value: Integer): Boolean; override;
    procedure DefaultDisposer(Value: Integer); override;
    function DefaultUpdater(Key: TKey; Value: TValue; IsNew: Boolean): Boolean; override;
  public
    MKey: Char;
    Max: Integer;
    constructor Create; override;
  end;

var
  i: Integer;
  c: Char;
  r : Cardinal;
  tp : THistogram;
  n: THistogram.PNode;

{ THistogram }

function THistogram.DefaultTraverser(Key: Char; Value: Integer): Boolean;
begin
  if Value > Max then begin
    MKey := Key;
    Max := Value;
  end;
  Write(Format('%s=%d'#9, [Key, Value]));
  Result := True;
end;

procedure THistogram.DefaultDisposer(Value: Integer);
begin
  WriteLn('Now disposing: ' + IntToStr(Value));
end;

function THistogram.DefaultUpdater(Key: TKey; Value: TValue; IsNew: Boolean): Boolean;
begin
  Result := (not IsNew) or (Value mod 2 = 0);
  if not Result then
    WriteLn(Format('Assigning odd number %d to %s is forbidden', [Value, Key]));
end;

constructor THistogram.Create;
begin
  inherited Create;
  Max := -1;
end;

begin
  tp := THistogram.Create;
  WriteLn('Inserting A~Z to Treap, assigning random values...');
  for c := 'A' to 'Z' do begin
    tp.Insert(c, Random(100));
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
  tp.Walk;
  WriteLn;
  WriteLn(Format('Max=%d at %s', [tp.Max, tp.MKey]));
  n := tp['X'];
  if n = nil then
    WriteLn('X is Not Found')
  else
    WriteLn('X is ' + IntToStr(n^.Value));
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
  tp.Max := -1;
  tp.Walk(1);
  WriteLn;
  WriteLn(Format('Max=%d at %s', [tp.Max, tp.MKey]));
  tp.Clear;
  WriteLn(Format('After Clear(), # of items=%d', [tp.Count]));
  tp.Free;
end.
