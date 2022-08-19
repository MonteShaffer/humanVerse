


sourceLocal = function()
	{
	suppressError_ = function(expression)
		{	
		try( expression , silent = TRUE);
		}
	is.error_ = function(e, where="suppressError")
		{
		condition = attributes(e)$condition;
		if(is.null(condition)) { return(FALSE); }	
		extra = attributes(condition)$class;
		if("error" %in% extra) { return(TRUE); }
		return(FALSE);
		}	
		
	res = list(); 
	setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R");
	# source("functions-str.R");
	file.sources = list.files(pattern="*.R");
	for(file.source in file.sources)
		{
		cat("\n", "=== ", file.source, " ===", "\n");
		x = suppressError_( source(file.source) );
		if(is.error_(x)) 
			{ 
			cat("\n"); print(x); cat("\n\n"); 
			res[[file.source]] = x;
			}
		}
	res;
	
	
	
	
	invisible(res);
	}


# using libcurl via library curl
# handle_setopt
# handle_create
# h <- new_handle()
# file:///C:/_git_/-downloads-/curl_4.3.2.tar/curl_4.3.2/curl/inst/doc/intro.html
# handle_setopt(h, copypostfields = "moo=moomooo");
# handle_setheaders(h,
  # "Content-Type" = "text/moo",
  # "Cache-Control" = "no-cache",
  # "User-Agent" = "A cow"
# )
# curl_options()
# https://curl.se/libcurl/c/CURLOPT_HEADERFUNCTION.html
# headerfunction	20079 in curl_options... weird ... 
# $ch = curl_init();
# curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
# curl_setopt($ch, CURLOPT_HEADER, 1);
# // ...

# $response = curl_exec($ch);

	
curl.exec = function(ch, method="curl_fetch_memory")
	{
	
	}
	
curl.init = function()
	{
	if(!is.library("curl")) { stop("this requires library(curl) to access libcurl"); }
	# register in memory ... append to handle timestamp, md5 or something
	# like timers ... ??? multiple handles ??
	ch = curl::new_handle();
	}
	
curl_init = curl.init;

curl.isHandle = function(ch)
	{
	res = inherits(ch, "curl_handle");
	if(!res) { stop("You don't have a [ch], maybe try ch = curl.init(); ");}
	}


curl.optionsToJSON = function() {}
curl.optionsFromJSON = function() {}
# https://stackoverflow.com/questions/5356075/how-to-get-an-option-previously-set-with-curl-setopt
# https://stackoverflow.com/a/5356184/184614
# wrap it up ... 
curl.setopt = function(ch, KEY, val)
	{	
	curl.isHandle(ch);
	key = as.character(substitute(KEY));
	key = tolower( str.replace("CURLOPT_", "", key) );
	keycode = as.integer( curl::curl_options()[key] );
	valcode = list( key = val );
	
	if(is.na(keycode)) { stop("bad KEY"); }
	
	
	.Call(curl:::R_handle_setopt, ch, keycode, valcode);
	
	invisible(ch);	
	}
	
curl_setopt = curl.setopt;


# curl::curl_symbols("CURLUSESSL")
# curl::curl_version()$protocols
# scp/sftp ... build rsync tool?

# https://stackoverflow.com/questions/52622155/c-libcurl-how-to-accept-expired-certificate-using-libcurl
## req = curl_fetch_memory("https://www.mshaffer.com/")
## req = curl_fetch_memory("https://www.mshaffer.com/");
# parse_headers(req$headers)
## ***** parse_headers(req$headers, multiple = TRUE) # add my lookup to this 
## str(req) ... has timings... curl_fetch_memory has binary of header/content 
## wrap into url.download as method ... 
# Parse into named list
# parse_headers_list(req$headers)


info = curlGetHeaders("http://www.mshaffer.com/");


handle_setopt = function (handle, ..., .list = list()) 
{
  stopifnot(inherits(handle, "curl_handle"))
  values <- c(list(...), .list)
  cat("\n", " ==== VALUES === ", "\n");
  print(values);
  opt_names <- curl:::fix_options(tolower(names(values)))
  keys <- as.integer(curl::curl_options()[opt_names])
  cat("\n", " ==== KEYS === ", "\n");
  print(keys);
  na_keys <- is.na(keys)
  if (any(na_keys)) {
    bad_opts <- opt_names[na_keys]
    stop("Unknown option", ifelse(length(bad_opts) > 1, 
      "s: ", ": "), paste(bad_opts, collapse = ", "))
  }
  stopifnot(length(keys) == length(values))
  .Call(curl:::R_handle_setopt, handle, keys, values)
  invisible(handle)
}


