unit FileList;
{$mode objfpc}{$H+}
interface
uses Classes, SysUtils;
type
  TFileAttr = (ftReadOnly, ftHidden, ftSystem, ftVolumeID, ftDirectory,
               ftArchive, ftNormal);
  TFileAttrSet = set of TFileAttr;
  TFileList = class(TStringList)
  private
    FRecursive : Boolean;
    FMask : String;
    FRoot : String;
    FTypes : TFileAttrSet;
    procedure GetFileNames(Dir: String);
  public
    property Recursive: Boolean read FRecursive write FRecursive;
    property Types: TFileAttrSet read FTypes write FTypes;
    property Mask: String read FMask write FMask;
    property Root: String read FRoot write FRoot;
    constructor Create;
    constructor Create(ADir: string; Recurse: Boolean=False; AMask: string='*';
      ATypes: TFileAttrSet = [ftReadOnly, ftArchive, ftNormal]);
    procedure Update;
  end;

implementation
constructor TFileList.Create;
begin
  FTypes := [ftReadOnly, ftArchive, ftNormal];
  FMask := '*';
  FRecursive := True;
  FRoot := '';
end;

procedure TFileList.GetFileNames(Dir: String); platform;
var
  AttrIndex: TFileAttr;
  MaskPtr: PChar;
  Ptr: PChar;
  AttrWord: Word;
  sr, sr1: TSearchRec;
const
   attr: array[TFileAttr] of Word = (faReadOnly, faHidden, faSysFile,
     faVolumeID, faDirectory, faArchive, 0);
begin
  AttrWord := 0;
  { Set attribute flags based on values in FileType }
  for AttrIndex := ftReadOnly to ftArchive do
    if AttrIndex in FTypes then AttrWord := AttrWord or attr[AttrIndex];
  Dir := IncludeTrailingPathDelimiter(Dir);
  MaskPtr := PChar(FMask);
  while MaskPtr <> nil do begin
    Ptr := StrScan(MaskPtr, ';');
    if Ptr <> nil then Ptr^ := #0;
    if FindFirst(Dir + MaskPtr, AttrWord, sr) = 0 then begin
      repeat            { exclude normal files if ftNormal not set }
        if (sr.Name = '.') or (sr.Name = '..') then Continue;
        if (ftNormal in FTypes) or (sr.Attr and AttrWord <> 0) then
          Add(ExpandFileName(Dir + sr.Name));
      until FindNext(sr) <> 0;
      FindClose(sr);
    end; 
    if FRecursive then if FindFirst(Dir+'*', faDirectory, sr1) = 0 then begin
      repeat
        if (sr1.Name = '.') or (sr1.Name = '..') then Continue;
        if (sr1.Attr and faDirectory <> 0) then GetFileNames(Dir+sr1.Name);
      until FindNext(sr1) <> 0;
      FindClose(sr1);
    end;
    if Ptr <> nil then begin
      Ptr^ := ';';
      Inc(Ptr);
    end;
    MaskPtr := Ptr;
  end;
end;

procedure TFileList.Update;
var
  ml : TStrings;
  i : Integer;
  OrgMask : string;
begin
  OrgMask := FMask;
  Clear;
  ml := TStringList.Create;
  ml.Delimiter := ';';
  ml.DelimitedText := FMask;
  for i := 0 to ml.Count - 1 do begin
    FMask := Trim(ml[i]);
    GetFileNames(FRoot);
  end;
  ml.Free;
  FMask := OrgMask;
end;

constructor TFileList.Create(ADir: string; Recurse: Boolean; AMask: string;
  ATypes: TFileAttrSet);
begin
  Root := ADir;
  Recursive := Recurse;
  Mask := AMask;
  Types := ATypes;
  Update;
end;

end.