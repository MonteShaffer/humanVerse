


#' http.status
#'
#' @param headers a string vector from 'http.headers'
#'
#' @return
#' @export
#'
#' @examples
#' http.status(http.headers("https://www.mshaffer.com/"));
http.status = function(headers)  ## BAD connection if NULL?
  {
  # requires libcurl
  # attributes(headers)$status;  # getAttribute
	property.get(headers, "status");
  }


#' http.headerValue
#'
#' @param headers a string vector from 'http.headers'
#' @param which
#'
#' @return
#' @export
#'
#' @examples
#' headers = http.headers("https://www.mshaffer.com/");
#' http.headerValue(headers, "Content-Type");
#' http.headerValue(headers, "Server");
http.headerValue = function(headers, search="Content-Length:")
  {
  # requires libcurl
  # "Content-Length: 30528\r\n"
  if(lastChar(search) != ":") { search = paste0(search,":"); }

  for(cheader in rev(headers)) # reverse so we get past redirects
    {
    cheader = str.trim(cheader);
    if(cheader != "")
      {
      # cat("\n", "header: ", cheader, " ... search: ", search, "\n");
      if(str.contains(search, cheader))  ## haystack, needle
        {
        value = str.trim( str.replace(search, "", cheader) );
        return( value );
        }
      }
    }
  FALSE;
  }



#' http.size
#'
#' @param headers a string vector from 'http.headers'
#'
#' @return The size of the page 'Content-Length'
#' @export
#'
#' @examples
#' http.size(http.headers("https://www.mshaffer.com/"));
#' http.size(http.headers("http://www.myprofiletraits.com/"));
http.size = function(headers)
  {
  # requires libcurl
  # "Content-Length: 30528\r\n"
  as.integer( http.headerValue(headers, "Content-Length:") );
  }





#' http.headers
#'
#' @param remote A url
#'
#' @return a string vector [headers]
#' @export
#'
#' @examples
#' http.headers("https://www.mshaffer.com/");
#' http.headers("https://www.myprofiletraits.com/");
http.headers = function(remote, ...)  # maybe verify=FALSE (expired https)
  {
  # requires libcurl in R::base
  	

	url.info = tryCatch	(

						{
						info = curlGetHeaders(remote, ...);
						},

						warning = function(w)
							{
							warning(paste0("### WARNING ###  throws a warning","\n\n",w));
							info; # let's still return the value 	.
							},
	
						error = function(e)
							{
							# warning(paste0("### ERROR ###  throws an error","\n\n",e));
							res = FALSE;
							res = property.set(res, "ERROR", e);
							return (res);
							},

						finally =
							{
				
							}
						);
	url.info;
	
  }


