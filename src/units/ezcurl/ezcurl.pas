unit ezcurl;
{$mode objfpc}
interface
type
  PCURL = Pointer;
{$I intf/curl_code.inc}
{$I intf/curl_opts.inc}
{$I intf/curl_info.inc}
{$I intf/curl_form.inc}
{$I intf/curl_intf.inc}
type
  TECDataBlock = record
    data: Pointer;
    size: Integer;
  end;
  PECDataBlock = array of TECDataBlock;
  TEasyCURL = class
  private
    _formadd: PF_CURL_FORMADD;
    FHandle: PCURL;
    FLastError: TCURLcode;
    FDataBlocks: PECDataBlock;
    procedure DisposeDataBlocks;
  public
    property formadd: PF_CURL_FORMADD read _formadd;
    procedure formfree(form: PCURL_HTTPPOST);
    function init: Boolean;
    function setopt(opt_n: TCURLoption; opt_v: LongInt): TCURLcode;
    function setopt(opt_n: TCURLoption; opt_v: Pointer): TCURLcode;
    function setopt(opt_n: TCURLoption; opt_v: PChar): TCURLcode;
    function perform: TCURLcode;
    function getinfo(info: TCURLINFO; data: Pointer): TCURLcode;
    function version: string;
    procedure cleanup;
    procedure recv_data(ptr: Pointer; size: Integer);
    function GetData: PECDataBlock;
    function GetDataAsString: AnsiString;
    function SaveDataToFile(filename: string): Integer;
    function LastError: TCURLcode;
    //function LastErrorMessage: string;
    constructor Create;
    destructor Destroy; override;
  end;
implementation
{$linklib curl}
function write_data(ptr: Pointer; size, nmemb: Integer; userdata: Pointer): Integer; cdecl;
var
  ec : TEasyCURL;
begin
  size := size * nmemb;
  ec := TEasyCURL(userdata);
  ec.recv_data(ptr, size);
  Result := size;
end;

procedure TEasyCURL.formfree(form: PCURL_HTTPPOST);
begin
  curl_formfree(form);
end;

function TEasyCURL.init: Boolean;
begin
  cleanup;
  FHandle := curl_easy_init;
  Result := Assigned(FHandle);
end;

function TEasyCURL.setopt(opt_n: TCURLoption; opt_v: LongInt): TCURLcode;
begin
  FLastError := curl_easy_setopt(FHandle, opt_n, opt_v);
  Result := FLastError;
end;

function TEasyCURL.setopt(opt_n: TCURLoption; opt_v: Pointer): TCURLcode;
begin
  FLastError := curl_easy_setopt(FHandle, opt_n, opt_v);
  Result := FLastError;
end;

function TEasyCURL.setopt(opt_n: TCURLoption; opt_v: PChar): TCURLcode;
begin
  FLastError := curl_easy_setopt(FHandle, opt_n, opt_v);
  Result := FLastError;
end;

function TEasyCURL.perform: TCURLcode;
begin
  if FLastError <> CURLE_OK then Exit;
  FLastError := curl_easy_setopt(FHandle, CURLOPT_WRITEFUNCTION, Pointer(@write_data));
  if FLastError <> CURLE_OK then Exit;
  FLastError := curl_easy_setopt(FHandle, CURLOPT_WRITEDATA, Self);
  if FLastError <> CURLE_OK then Exit;
  DisposeDataBlocks;
  FLastError := curl_easy_perform(FHandle);
  Result := FLastError;
end;

function TEasyCURL.getinfo(info: TCURLINFO; data: Pointer): TCURLcode;
begin
  FLastError := curl_easy_getinfo(FHandle, info, data);
  Result := FLastError;
end;

function TEasyCURL.version: string;
begin
  Result := curl_version;
end;

procedure TEasyCURL.cleanup;
begin
  if Assigned(FHandle) then curl_easy_cleanup(FHandle);
end;

procedure TEasyCURL.DisposeDataBlocks;
var
  i: Integer;
begin
  for i := 0 to Length(FDataBlocks) - 1 do
    FreeMem(FDataBlocks[i].data);
  SetLength(FDataBlocks, 0);
end;

procedure TEasyCURL.recv_data(ptr: Pointer; size: Integer);
var
  c: Integer;
begin
  c := Length(FDataBlocks);
  SetLength(FDataBlocks, c + 1);
  FDataBlocks[c].size := size;
  FDataBlocks[c].data := GetMem(size);
  Move(ptr^, FDataBlocks[c].data^, size);
end;

function TEasyCURL.GetData: PECDataBlock;
begin
  Result := FDataBlocks;
end;

function TEasyCURL.GetDataAsString: AnsiString;
var
  c, i, o : Integer;
  buf: PChar;
begin
  c := 0;
  for i := 0 to Length(FDataBlocks) - 1 do 
    Inc(c, FDataBlocks[i].Size);
  buf := GetMem(c + 1);
  FillChar(buf^, c + 1, 0);
  o := 0;
  for i := 0 to Length(FDataBlocks) - 1 do begin
    Move(FDataBlocks[i].data^, PChar(buf + o)^, FDataBlocks[i].size);
    Inc(o, FDataBlocks[i].size);
  end;
  Result := buf;
end;

function TEasyCURL.SaveDataToFile(filename: string): Integer;
var
  f : file;
  c, i : Integer;
begin
  Result := 0;
  if Length(FDataBlocks) = 0 then Exit;
  AssignFile(f, filename);
  Rewrite(f, 1);
  for i := 0 to Length(FDataBlocks) - 1 do begin
    BlockWrite(f, FDataBlocks[i].data^, FDataBlocks[i].size, c{%H-});
    Inc(Result, c);
  end;
  CloseFile(f);
end;

function TEasyCURL.LastError: TCURLcode;
begin
  Result := FLastError;
end;

constructor TEasyCURL.Create;
begin
  _formadd := @curl_formadd;
  FHandle := nil;
  FLastError := CURLE_OK;
  SetLength(FDataBlocks, 0);
end;

destructor TEasyCURL.Destroy;
begin
  cleanup;
  DisposeDataBlocks;
end;

end.
