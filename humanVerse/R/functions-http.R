

http.headers = function(remote)
  {
  # requires libcurl
  curlGetHeaders(remote);
  }


http.status = function(headers)
  {
  # requires libcurl
  attributes(headers)$status;  # getAttribute
  }


http.headerValue = function(headers, which="Content-Length:")
  {
  # requires libcurl
  # "Content-Length: 30528\r\n"
  for(header in rev(headers)) # reverse so we get past redirects
    {
    if(is.substring(header, which))
      {
      size = trimMe( gsub(which, "", header, fixed=TRUE) );
      return( as.integer( size ) );
      }
    }
  FALSE;
  }

http.size = function(headers)
  {
  # requires libcurl
  # "Content-Length: 30528\r\n"
  http.headerValue(headers, "Content-Length:");
  }




