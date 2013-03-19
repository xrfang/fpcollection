type
  PF_CURL_FORMADD = function(firstitem, lastitem: PPCURL_HTTPPOST): TCURLFORMcode; cdecl varargs;
  
function curl_version: PChar; cdecl external;
function curl_easy_init: PCURL; cdecl external;
procedure curl_easy_cleanup(handle: PCURL); cdecl external;
function curl_easy_setopt(handle: PCURL; opt_n: TCURLoption; opt_v: LongInt): TCURLcode; cdecl external;
function curl_easy_setopt(handle: PCURL; opt_n: TCURLoption; opt_v: Pointer): TCURLcode; cdecl external;
function curl_easy_perform(handle: PCURL): TCURLcode; cdecl external;
function curl_easy_getinfo(handle: PCURL; info: TCURLINFO; data: Pointer): TCURLcode; cdecl external;
function curl_formadd(firstitem, lastitem: PPCURL_HTTPPOST): TCURLFORMcode; cdecl varargs external;
procedure curl_formfree(form: PCURL_HTTPPOST); cdecl external;
//function curl_global_init(LongInt flags): Integer; cdecl external;

