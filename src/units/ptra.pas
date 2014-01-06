unit ptra;
{$mode objfpc}{$H+}
interface
uses types;
function EncPtrA(arr: TPointerDynArray): Pointer;
function DecPtrA(ptr: Pointer): TPointerDynArray;
function RelPtrA(ptr: Pointer): Integer;

implementation
const
  PTRA_SIG = 'PtrA';

function NotPtrA(ptr: Pointer): Boolean;
var
  i: Integer;
begin
  for i := 0 to Length(PTRA_SIG) - 1 do
    if Char(Pointer(ptr + i)^) <> PTRA_SIG[i + 1] then Exit(True);
  Result := False;
end;

function EncPtrA(arr: TPointerDynArray): Pointer;
var
  len, size: Integer;
  ptr: Pointer;
begin
  len := Length(arr);
  if len = 0 then Exit(nil);
  size := Length(PTRA_SIG) + SizeOf(Integer) + len * SizeOf(Pointer);
  Result := GetMem(size);
  ptr := Result;
  Move(PTRA_SIG, ptr^, Length(PTRA_SIG));
  ptr += Length(PTRA_SIG);
  Move(len, ptr^, SizeOf(Integer));
  ptr += SizeOf(Integer);
  Move(arr[0], ptr^, len * SizeOf(Pointer));
end;

function DecPtrA(ptr: Pointer): TPointerDynArray;
var
  len: Integer;
begin
  if ptr = nil then Exit(nil);
  if NotPtrA(ptr) then Exit(nil);
  ptr += Length(PTRA_SIG);
  len := Integer(ptr^);
  ptr += SizeOf(Integer);
  SetLength(Result, len);
  Move(ptr^, Result[0], len * SizeOf(Pointer));
end;

function RelPtrA(ptr: Pointer): Integer;
var
  len: Integer;
begin
  if ptr = nil then Exit(0);
  if NotPtrA(ptr) then Exit(-1);
  len := Integer(Pointer(ptr + Length(PTRA_SIG))^);
  Result := Length(PTRA_SIG) + SizeOf(Integer) + len * SizeOf(Pointer);
  FreeMem(ptr, Result);
end;

end.

