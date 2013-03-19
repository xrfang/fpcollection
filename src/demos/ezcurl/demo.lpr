program demo;
{$mode objfpc}{$H+}
uses ezcurl, SysUtils;
var
  url : PChar;
  curl : TEasyCURL;
  res : TCURLcode;
  size : Integer;
  info : PChar;
  code : Integer;
  post, last: PCURL_HTTPPOST;
begin
  if ParamCount <> 1 then begin
    WriteLn('USAGE: ', ExtractFileName(ParamStr(0)), ' <url>');
    Exit;
  end;
  url := GetMem(Length(ParamStr(1)) + 1);
  StrPCopy(url, ParamStr(1));
  curl := TEasyCURL.Create;
  WriteLn('CURL version: ' + curl.version);
  if curl.init then begin
    curl.setopt(CURLOPT_URL, url);
    curl.setopt(CURLOPT_SSL_VERIFYPEER, 0);
    curl.setopt(CURLOPT_SSL_VERIFYHOST, 0);
    curl.formadd(@post, @last, CURLFORM_COPYNAME, '_t', CURLFORM_COPYCONTENTS,
      'token', CURLFORM_END);
    curl.setopt(CURLOPT_HTTPPOST, post);
    res := curl.perform;
    WriteLn(res);
    if res = CURLE_OK then begin
      WriteLn(curl.GetDataAsString);
      WriteLn;
      size := curl.SaveDataToFile('curl_demo.txt');
      WriteLn('Saved to file "curl_demo.txt", size: ' , size);
      if CURLE_OK = curl.getinfo(CURLINFO_RESPONSE_CODE, @code) then
        WriteLn('Response code: ', code);
      if CURLE_OK = curl.getinfo(CURLINFO_CONTENT_TYPE, @info) then
        WriteLn('Content-Type: ', info);
    end;
    curl.formfree(post);
    curl.Free;
  end;
end.
