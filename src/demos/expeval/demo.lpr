program demo;
{$mode objfpc}{$H+}
uses sysutils, expeval;
var
  i : Integer;
  ee: TExpEval;
begin
  ee := TExpEval.Create;
  ee.Vars.Add('x');  //Vars must be defined BEFORE Formula
  ee.Formula := '3 * x ^ 2';
  for i := -9 to 9 do WriteLn(Format('%.0f', [ee.Calc([i])]));
  ee.Free;
end.
