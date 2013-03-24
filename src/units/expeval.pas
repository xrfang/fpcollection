(**
 * Adapted from CalcExpress
 * http://www.aidaim.com/products/free_components/intr_spc.php
 *   - works with real numbers;
 *   - accepts operators: + - * / ^;
 *   - accepts functions: cos, sin, tg, ctg, abs, sgn(sign), sqrt, ln, exp,
 *     arcsin, arcos, arctg(arctan), arcctg, sh(sinh), ch(cosh), th(tanh),
 *     cth(coth), heaviside;
 *   - supports unlimited number of user defined variables.
 *)
unit expeval;
{$mode objfpc}{$H+}
interface
uses SysUtils, Classes, Math;
  
type
  TExpEval = class
  private
  type
    PTree = ^TTree;
    TTree = record
      num: Integer;
      con: string;
      l, r: Pointer;
    end;
  private
    Err: Boolean;
    Bc: Integer;
    PrevLex, CurLex: Integer;
    Pos: Integer;
    FFormula: string;
    Tree: Pointer;
    FVariables: TStrings;
    FDefaultNames: Boolean;
    procedure Init(s: string);
    function GetTree(s: string): Pointer;
    function DelTree(t: PTree): Pointer;
    procedure Error(s: string);
    procedure SetVars(Value: TStrings);
  public
    property Formula: string read FFormula write Init;
    property Vars: TStrings read FVariables write SetVars;
    constructor Create;
    destructor Destroy; override;
    function Calc(args: array of Extended): Extended;
    function Calc: Extended;
  end;

implementation

function TExpEval.Calc(args: array of Extended): Extended;
  function c(t: PTree): Extended;
  var 
    r: Extended;
  begin
    c := 0;
    case t^.num of
      3: c := c(t^.l) + c(t^.r);
      4: c := c(t^.l) - c(t^.r);
      5: c := c(t^.l) * c(t^.r);
      6: c := c(t^.l) / c(t^.r);
      7: c := strtofloat(t^.con);
      8: c := args[StrToInt(t^.con)];
      9: c := -c(t^.l);
      10: c := cos(c(t^.l));
      11: c := sin(c(t^.l));
      12: c := tan(c(t^.l));
      13: c := 1 / tan(c(t^.l));
      14: c := abs(c(t^.l));
      15: begin
        r := c(t^.l);
        if r < 0 then c := -1 
        else if r > 0 then c := 1 
        else c := 0;
      end;
      16: c := sqrt(c(t^.l));
      17: c := ln(c(t^.l));
      18: c := exp(c(t^.l));
      19: c := arcsin(c(t^.l));
      20: c := arccos(c(t^.l));
      21: c := arctan(c(t^.l));
      22: c := pi / 2 - arctan(c(t^.l));
      23: begin
        r := c(t^.l);
        c := (exp(r) - exp(-r)) / 2;
      end;
      24: begin
        r := c(t^.l);
        c := (exp(r) + exp(-r)) / 2;
      end;
      25: begin
        r := c(t^.l);
        c := (exp(r) - exp(-r)) / (exp(r) + exp(-r));
      end;
      26: begin
        r := c(t^.l);
        c := (exp(r) + exp(-r)) / (exp(r) - exp(-r));
      end;
      27: begin
        r := c(t^.l);
        if r >= 0 then c := 1 else c := 0;
      end;
      31: c := power(c(t^.l), c(t^.r));
    end;
  end;
begin
  Result := c(tree);
end;

function TExpEval.Calc: Extended;
begin
  Result := Calc([]);
end;

procedure TExpEval.Error(s: string);
begin
  Err := True;
  raise Exception.Create(s);
end;

constructor TExpEval.Create;
begin
  Tree := nil;
  Formula := '0';
  FDefaultNames := False;
  FVariables := TStringList.Create;
end;

destructor TExpEval.Destroy;
begin
  DelTree(Tree);
  FVariables.Free;
end;

function TExpEval.GetTree(s: string): Pointer;
var
  neg: Boolean;
  l, r, res: PTree;
  n, op: Integer;
  c: string;
  function GetNumber(s: string): string;  //Get number from string
  begin
    Result := '';
    try //Begin
      while (pos <= Length(s)) and (s[pos] in ['0'..'9']) do begin
        Result := Result + s[pos];
        pos += 1;
      end;
      if pos > Length(s) then Exit;
      if s[pos] = DefaultFormatSettings.DecimalSeparator then begin //Fraction part
        Result += DefaultFormatSettings.DecimalSeparator;
        pos += 1;
        if (pos > Length(s)) or not (s[pos] in ['0'..'9']) then Error('Wrong number.');
        while (pos <= Length(s)) and (s[pos] in ['0'..'9']) do begin
          Result += s[pos];
          pos += 1;
        end;
      end;
      if pos > Length(s) then Exit;
      if (s[pos] <> 'e') and (s[pos] <> 'E') then Exit; //Power
      Result += s[pos];
      pos += 1;
      if pos > Length(s) then Error('Wrong number.');
      if s[pos] in ['-', '+'] then begin
        Result += s[pos];
        pos += 1;
      end;
      if (pos > Length(s)) or not (s[pos] in ['0'..'9']) then Error('Wrong number.');
      while (pos <= Length(s)) and (s[pos] in ['0'..'9']) do begin
        Result += s[pos];
        pos += 1;
      end;
    except
    end;
  end;
  procedure GetLex(s: string; var num: Integer; var con: string);  //Read lexem from string
  begin
    con := '';
    while (pos <= Length(s)) and (s[pos] = ' ') do pos += 1; //skip spaces
    if pos > Length(s) then begin
      num := 0;  
      Exit;
    end;
    case s[pos] of
      '(': num := 1;
      ')': num := 2;
      '+': num := 3;
      '-': begin
        num := 4;
        if (pos < Length(s)) and (s[pos + 1] in ['1'..'9', '0']) and
          (CurLex in [0,1]) then begin
          pos += 1;
          con := '-' + GetNumber(s);
          pos -= 1;
          num := 7;
        end;
      end;
      '*': num := 5;
      '/': num := 6;
      '^': num := 31;
      'a'..'z', 'A'..'Z', '_': begin
        while (pos <= Length(s)) and
          (s[pos] in ['a'..'z', 'A'..'Z', '_', '1'..'9', '0']) do begin
          con += s[pos];
          pos += 1;
        end;
        pos -= 1;
        num := 8;
        if con = 'cos' then num := 10;
        if con = 'sin' then num := 11;
        if con = 'tg' then num := 12;
        if con = 'ctg' then num := 13;
        if con = 'abs' then num := 14;
        if (con = 'sgn') or (con = 'sign') then num := 15;
        if con = 'sqrt' then num := 16;
        if con = 'ln' then num := 17;
        if con = 'exp' then num := 18;
        if con = 'arcsin' then num := 19;
        if con = 'arccos' then num := 20;
        if (con = 'arctg') or (con = 'arctan') then num := 21;
        if con = 'arcctg' then num := 22;
        if (con = 'sh') or (con = 'sinh') then num := 23;
        if (con = 'ch') or (con = 'cosh') then num := 24;
        if (con = 'th') or (con = 'tanh') then num := 25;
        if (con = 'cth') or (con = 'coth') then num := 26;
        if (con = 'heaviside') or (con = 'h') then num := 27;
        if num = 8 then con := IntToStr(FVariables.IndexOf(con));
      end;
      '1'..'9', '0': begin
        con := GetNumber(s);
        pos -= 1;
        num := 7;
      end;
    end;
    pos += 1;
    PrevLex := CurLex;
    CurLex := num;
  end;
  function NewNode: PTree;
  begin
    Result := AllocMem(sizeof(TTree));
    Result^.l := nil;
    Result^.r := nil;
  end;
  function GetSingleOp: Pointer;
  var 
    op, bracket: Integer;
    opc: string;
    l, r, res: PTree;
  begin
    l := nil;
    try
      if n = 1 then begin
        bc += 1;
        l := GetTree(s);
      end else begin // First operand
        if not (n in [7, 8, 10..30]) then Error('');
        op := n;
        opc := c;
        if n in [7, 8] then begin // Number or variable
          l := NewNode;
          l^.num := op; 
          l^.con := opc;
        end else begin //Function
          GetLex(s, n, c);
          if n <> 1 then Error('');
          bc += 1;
          l := NewNode;
          l^.l := GetTree(s);
          l^.num := op; 
          l^.con := opc;
        end;
      end;
      GetLex(s, n, c); //Operation symbol
      while n = 31 do begin //Power symbol
        GetLex(s, n, c);
        bracket := 0;
        if n = 1 then begin
          bracket := 1;   
          GetLex(s, n, c);
        end;
        if (n <> 7) and (n <> 8) then Error('');
        r := NewNode;
        r^.num := n; 
        r^.con := c;
        res := NewNode;
        res^.l := l; 
        res^.r := r; 
        res^.num := 31; 
        l := res;
        if bracket = 1 then begin
          GetLex(s, n, c);
          if n <> 2 then Error('');
        end;
        GetLex(s, n, c);
      end;
      Result := l;
    except
      DelTree(l);
      Result := nil;
    end;
  end;
  function GetOp: Pointer;
  var 
    op: Integer;
    l, r, res: PTree;
  begin
    neg := False;
    GetLex(s, n, c);
    // Unary - or +
    if PrevLex in [0, 1] then
    begin
      if n = 4 then begin
        neg := True; 
        GetLex(s, n, c);
      end;
      if n = 3 then GetLex(s, n, c);
    end;
    l := GetSingleOp;
    while n in [5, 6] do begin // 2nd operand
      op := n;
      GetLex(s, n, c);
      r := GetSingleOp;
      res := AllocMem(sizeof(TTree));
      res^.l := l; 
      res^.r := r; 
      res^.num := op;
      l := res;
    end;
    // Unary minus
    if neg then begin
      res := AllocMem(sizeof(TTree));
      res^.l := l; 
      res^.r := nil; 
      res^.num := 9;
      l := res;
    end;
    Result := l;
  end;
begin
  l := nil;
  try
    l := GetOp;
    while True do begin
      if n in [0, 2] then begin
        if n = 2 then dec(bc);
        Result := l; 
        Exit;
      end;
      if not (n in [3, 4]) then Error('');
      op := n;
      r := GetOp;
      res := AllocMem(sizeof(TTree));
      res^.l := l; 
      res^.r := r; 
      res^.num := op;
      l := res;
    end;
    Result := l;
  except
    DelTree(l);
    Result := nil;
  end;
end;

procedure TExpEval.Init(s: string);
begin
  DelTree(tree);
  Err := False;
  FFormula := LowerCase(s);
  PrevLex := 0;
  CurLex := 0;
  Pos := 1;  
  bc := 0;
  Tree := GetTree(Lowercase(s));
  if (bc <> 0) or Err then begin
    Tree := DelTree(Tree);
    Error('Invalid formula.');
  end;
end;

function TExpEval.DelTree(t: PTree): Pointer;
begin
  Result := nil;
  if t = nil then Exit;
  if t^.l <> nil then DelTree(t^.l);
  if t^.r <> nil then DelTree(t^.r);
  FreeMem(t);
end;

procedure TExpEval.SetVars(Value: TStrings);
begin
  FVariables.Clear;
  FVariables.Assign(Value);
  Init(Formula);
end;

end.
