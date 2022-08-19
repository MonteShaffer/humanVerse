

check.colorCapability = function()
	{
	
# https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r

	# term <- Sys.getenv()["TERM"]
  # colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  # if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    # return(text)
  # }
	
	}


check.ifConformable = function(x, y) {} # matrix?

check.isCompatibleLength = function(x, y, 
									method="equal",  # "1-1-equal"
									action="warning", 
									msg = " obj1 [x] and obj2 [y] are incompatible lengths, you may get spurious results."
								)
	{
	met = functions.cleanKey(method, 3, keep="-");
	acti = functions.cleanKey(action, 4);
	xlen = length(x);
	ylen = length(y);
	b = (ylen == xlen);  
		if(met == "equ") { return(TRUE); }
		
	xone = (xlen == 1);
	yone = (ylen == 1);	
		if( (met == "11e" || met == "1,1") && (xone || yone) )
			{
			return(TRUE);
			}
			
	if(acti == "warn") { warning(msg); }
	if(acti == "stop") { stop(msg); }
	}
						

# this literally "pings" the url via `base::curlGetHeaders()`
# if `base::url()` loads ...
check.url = function(urls, timeout=2, verify.certificate=FALSE,  ...)
	{
debug = FALSE;
	n = length(urls);
	res = logical(n);
	for(i in 1:n)
		{
		url = urls[i];
		conn = suppressError( url(url),
							show.notice=debug, msg="debug url of url.testConnection" 
							);
		on.exit(close(conn));
		if(is.error(conn)) { next; }

		# actually ping it 
		head = suppressError( base::curlGetHeaders(	url, 
													timeout=timeout, 
													verify=verify.certificate
													),
							show.notice=debug, msg="debug head of url.testConnection"
							);
		if(is.error(head)) { next; }
		
		res[i] = TRUE;
		}
	res;
	}
	
#' @rdname url.testConnection
#' @export
url.testConnection = check.url;
