
# https://stackoverflow.com/questions/9183178/can-php-curl-retrieve-response-headers-and-body-in-a-single-request
# looks like R doesn't unlike direct access to curl_opt
# 

urls = c("https://en.wikipedia.org/wiki/Columbia_Falls,_Montana",
		"https://www.mshaffer.com/hello/there.html?j=3&id=3309480",
		"https://stackoverflow.com/questions/695438/what-are-the-safe-characters-for-making-urls",
		"https://en.wikipedia.org/wiki/Columbia_Falls,_Montana?monte=says&hi=TRUE#Demographics", 
		"http://google.com"
		);

url.toPath = function() {}		
url.toPath = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana",
						w.chars = c("/", ",", "?"), 
						f.chars = c("^", "+", "^q^")	
						)
  {
  s = property.get("s", url);
  if(!is.null(s)) { out = s; vector.appendProperties(out, url); return(out); }
  
  # remove everything after the anchor tag("#")
  info = str.explode("#", url);
  nurl = list.getElements(info, 1);
  # set protocol ... http:// or https:// or ftp://
  info = str.explode("://", nurl);
  sep.http = "__{PROTOCOL}__";  # hard-coded so inverse REGEX will work 
  http_ = str.replace("{PROTOCOL}", list.getElements(info, 1), sep.http);
  more = list.getElements(info, 2);
  # maybe truncate (e.g., stack overflow) 
  more = str.replace(w.chars, f.chars, more);
  
  out = paste0(http_, more);
vector.appendProperties(out, url);
  }

folderizeURL = url.toPath;


url.fromPath = function() {}		
url.fromPath = function(path = "__https__en.wikipedia.org^wiki^Columbia_Falls+_Montana",
						w.chars = c("/", ",", "?"), 
						f.chars = c("^", "+", "^q^")	
						)
  {
  s = property.get("s", path);
  if(!is.null(s)) { out = s; vector.appendProperties(out, path); return(out); }
  
  npath = str.replace(rev(f.chars), rev(w.chars), path);
  
  # regex.wildcardSearch(npath, "__*__");
  pattern = "(_{2}){1}([^_]+){1}(_{2}){1}";
	info = regex.match(pattern, npath);
		http_ = list.getElements(info, 3);	
		http__ = paste0("__",http_,"__");

		out = str.replace(http__, paste0(http_,"://"), npath);
vector.appendProperties(out, path);  # stores original path, caching if reusing  
  }


defolderizeURL = url.fromPath;
unfolderizeURL = url.fromPath;
deFolderizeURL = url.fromPath;
unFolderizeURL = url.fromPath;

url.testConnection = function(urls, verify=FALSE, timeout=2, ...)
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
		if(is.error(conn)) { next; }

		# actually ping it 
		head = suppressError( curlGetHeaders(url, timeout=timeout, verify=verify),
							show.notice=debug, msg="debug head of url.testConnection"
							);
		if(is.error(head)) { next; }
		
		res[i] = TRUE;
		}
	res;
	}

 
# this is base function that wraps download.file ... 
# elsewhere we have to determine the paths ... 
# setwd("C:/_git_/-R-")
# paths = getwd();

# "2022-08-16 04:21:54 EDT" ... [13] "2022-08-17 10:28:39 EDT"
# > timer.start("bits"); x = prime.bits((1000*1000)); length(x); max(x); timer.stop("bits");
# [1] 1000000
# [1] 15485863

 # howMany:  9 

 # CASE 1 

 # RELATIVE TIME AT [ STOP-9 ]      1.25 seconds 

url.download = function(urls, paths, 
								use.cache=TRUE, 
								quiet=TRUE, 
								timeout=2, 
								verify=FALSE,
								method="base",
								mode = "curl_fetch_memory"
								...)
	{
debug = FALSE;
	n.urls = length(urls); n.paths = length(paths);
	if(n.paths == 1) { paths = rep(paths, n.urls); n.paths = n.urls; }  # many to one ...
	if(n.urls != n.paths) { stop("mismatch in lengths of urls/paths"); }
	
	urls_ = url.toPath(urls);
	folders = paste0(paths, "/", urls_, "/" );	# add trailing slash
	folders = str.replace("//", "/", folders)				# in case, trailing slash not needed
	n.max = max( strlen(folders) );
	if(n.max > 225) { stop("folder/filename lengths are getting long"); }
	
	
	## can I get headers and content at the same time ??? w/o curl ?
	errors = logical(n.urls);
	for(i in 1:n.urls)
		{
		url = urls[i];
		url_ = urls_[i];
		folder = folders[i];
		pname = "index.html";
		dir.createDirectoryRecursive(folder, verbose=FALSE);
		f.page = paste0(folder,pname); 	# no extension
		
		cat("\n", "   FILENAME: ", f.page, " \n");
		
		if(file.exists(f.page) && use.cache ) 
			{ 
			cat("\n", "\t\t\t ... not downloading, CACHED ", "\n");
			next; 
			}
		
		s = Sys.time();
		
		f.info = paste0(folder,"-info-"); 		# no extension
			cat( paste0("START:\t",s) , "\n", sep="", file=f.info, append=FALSE);
			cat( paste0("url:\t",url) , "\n", sep="", file=f.info, append=TRUE);
			cat( paste0("url_:\t",url_) , "\n", sep="", file=f.info, append=TRUE);
			cat( paste0("folder:\t",folder) , "\n", sep="", file=f.info, append=TRUE);
			cat( paste0("pname:\t",pname) , "\n", sep="", file=f.info, append=TRUE);
		f.head = paste0(folder,"-head-"); 		# no extension
		
		h = suppressError( curlGetHeaders(url, timeout=timeout, verify=verify),
							show.notice=debug, msg="debug head of url.download"
							);
		if(!is.error(h)) 
			{ 
			cat( h , "\n", sep="", file=f.head, append=FALSE);
			} else {
					msg = "\n ============ ERROR with HEADERS ============ \n";
					cat("\n", msg, "\n");
					cat(msg, "\n", sep="", file=f.info, append = TRUE); 
					cat( h , "\n", sep="", file=f.info, append=TRUE);
					errors[i] = TRUE;
					next;
					}
		
		
		
			# wrap in TRAP error? already caught with headers above 
			download.file(url, f.page, quiet=quiet, verify=verify, ...)
			 
		e = Sys.time();
		d = as.numeric(e) - as.numeric(s);
		
			cat( paste0("end:\t",e) , "\n", sep="", file=f.info, append=TRUE);
			cat( paste0("elapsed:\t",d) , "\n", sep="", file=f.info, append=TRUE);
		
		}
	
	# with libcurl, I could do multivariate without "blocking"
	# Support for method "libcurl" was optional on Windows prior to R 4.2.0
	# download.file(url, path, quiet=quiet...)
	
	res = folders;
	res = property.set("ERRORS", res, errors);
	invisible(res);
	}





# -header-
# -info- ... full url , time to download, etc. ... 
# index.html if the path has trailing slash ... otherwise page name 

