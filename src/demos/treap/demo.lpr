program demo;
{$mode objfpc}
uses sysutils, treap;
type

  { THistogram }

  THistogram = class(specialize TTreap<Char, Integer>)
  protected
    function Traverse(Key: Char; Value: Integer; UserData: Pointer): Boolean; override;
    procedure OnDispose(Value: Integer); override;
  public
    MKey: Char;
    Max: Integer;
    constructor Create; override;
  end;
var
  i : Integer;
  tp : THistogram;
  n: THistogram.PNode;

{ THistogram }

function THistogram.Traverse(Key: Char; Value: Integer; UserData: Pointer): Boolean;
begin
  if Value > Max then begin
    MKey := Key;
    Max := Value;
  end;
  Write(Format('%s=%d'#9, [Key, Value]));
  Result := True;
end;

procedure THistogram.OnDispose(Value: Integer);
begin
  WriteLn('Now disposing: ' + IntToStr(Value));
end;

constructor THistogram.Create;
begin
  inherited Create;
  Max := -1;
end;

{$R *.res}

begin
  tp := THistogram.Create;
  for i := Ord('A') to Ord('Z') do begin
    tp.Insert(Chr(i), Random(100));
  end;
  WriteLn('# of items: ' + IntToStr(tp.Count));
  tp.Walk;
  WriteLn;
  WriteLn(Format('Max=%d at %s', [tp.Max, tp.MKey]));
  n := tp.Find('X');
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
