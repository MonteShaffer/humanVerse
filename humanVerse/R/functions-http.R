

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
http.headers = function(remote)
  {
  # requires libcurl in R::base
  curlGetHeaders(remote);
  }


#' http.status
#'
#' @param headers a string vector from 'http.headers'
#'
#' @return
#' @export
#'
#' @examples
#' http.status(http.headers("https://www.mshaffer.com/"));
http.status = function(headers)
  {
  # requires libcurl
  attributes(headers)$status;  # getAttribute
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
    cheader = trimMe(cheader);
    if(cheader != "")
      {
      # cat("\n", "header: ", cheader, " ... search: ", search, "\n");
      if(is.substring(cheader, search))
        {
        value = trimMe( str_replace(search, "", cheader) );
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




