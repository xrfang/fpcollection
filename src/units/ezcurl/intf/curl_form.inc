const
  HTTPPOST_FILENAME    = 1 << 0;  // specified content is a file name
  HTTPPOST_READFILE    = 1 << 1;  // specified content is a file name
  HTTPPOST_PTRNAME     = 1 << 2;  // name is only stored pointer, do not free in formfree
  HTTPPOST_PTRCONTENTS = 1 << 3;  // contents is only stored pointer, do not free in formfree
  HTTPPOST_BUFFER      = 1 << 4;  // upload file from buffer
  HTTPPOST_PTRBUFFER   = 1 << 5;  // upload file from pointer contents
  HTTPPOST_CALLBACK    = 1 << 6;  { upload file contents by using the regular read
                                    callback to get the data and pass the given pointer
                                    as custom pointer }
type
  TCURLformoption = (
    CURLFORM_NOTHING,         // the first one is unused

    CURLFORM_COPYNAME,
    CURLFORM_PTRNAME,
    CURLFORM_NAMELENGTH,
    CURLFORM_COPYCONTENTS,
    CURLFORM_PTRCONTENTS,
    CURLFORM_CONTENTSLENGTH,
    CURLFORM_FILECONTENT,
    CURLFORM_ARRAY,
    CURLFORM_OBSOLETE,
    CURLFORM_FILE,

    CURLFORM_BUFFER,
    CURLFORM_BUFFERPTR,
    CURLFORM_BUFFERLENGTH,

    CURLFORM_CONTENTTYPE,
    CURLFORM_CONTENTHEADER,
    CURLFORM_FILENAME,
    CURLFORM_END,
    CURLFORM_OBSOLETE2,

    CURLFORM_STREAM,

    CURLFORM_LASTENTRY        // the last unused
  );

  TCURLFORMcode = (
    CURL_FORMADD_OK,          // first, no error

    CURL_FORMADD_MEMORY,
    CURL_FORMADD_OPTION_TWICE,
    CURL_FORMADD_NULL,
    CURL_FORMADD_UNKNOWN_OPTION,
    CURL_FORMADD_INCOMPLETE,
    CURL_FORMADD_ILLEGAL_ARRAY,
    CURL_FORMADD_DISABLED,   // libcurl was built with this disabled

    CURL_FORMADD_LAST        // last
  );

  PPCURL_HTTPPOST = ^PCURL_HTTPPOST;
  PCURL_HTTPPOST = ^TCURL_HTTPPOST;
  TCURL_HTTPPOST = record
    next: PCURL_HTTPPOST;        // next entry in the list
    name: PChar;                 // pointer to allocated name
    namelength: LongInt;         // length of name length
    contents: PChar;             // pointer to allocated data contents 
    contentslength: LongInt;     // length of contents field 
    buffer: PChar;               // pointer to allocated buffer contents 
    bufferlength: LongInt;       // length of buffer field 
    contenttype: PChar;          // Content-Type 
    contentheader: PCURL_SLIST;  // list of extra headers for this form 
    more: PCURL_HTTPPOST;         { if one field name has more than one file,
                                    this link should link to following files }
    flags: LongInt;              // const HTTPPOST_*, as defined above
    showfilename: PChar;          { The file name to show. If not set, the
                                    actual file name will be used (if this
                                    is a file part) }
    userp: Pointer;              // custom pointer used for HTTPPOST_CALLBACK posts 
  end;

