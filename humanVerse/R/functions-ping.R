

check.internet = function(domain="8.8.8.8")
	{
	ping.domain(domain, 1);
	}
  
ping.domain = function(domain="humanVerse.today", count=1, 
							intern = TRUE, ...
						)
	{   
	args = function.arguments("main");
	flag = "c"; if(is.windows()) { flag = "n"; args$windows = TRUE; }
	args$time$tz = Sys.timezone();
	args$time$start = as.numeric(Sys.time());  # as.POSIXct , tz="UTC");  # UTC doesn't work   
	
	## windows ... ping -n 5 google.com
	## debian ... ping -c 5 google.com	
	cmd = paste0("ping -", flag, " ",count, " ", domain);	
	args$cmd = cmd;
	
cat("\n\n\n\t\t ping.domain :: ", cmd, "\n\n\n");
	res = system( cmd , intern = intern, ...);
	args$time$end = as.numeric(Sys.time());   # as.POSIXct tz="GMT");  # GMT doesn't work 
	res = property.set("args", res, args);
#dput(res);  # don't parse on NO DATA???
	ping.parse(res);
	}  

 
ping.parse = function(res)
	{
#dput(res);
	# res = structure(c("", "Pinging humanVerse.today [66.29.141.54] with 32 bytes of data:", "Reply from 66.29.141.54: bytes=32 time=93ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=90ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=199ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=90ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=190ms TTL=49", "", "Ping statistics for 66.29.141.54:", "    Packets: Sent = 5, Received = 5, Lost = 0 (0% loss),", "Approximate round trip times in milli-seconds:",  "    Minimum = 90ms, Maximum = 199ms, Average = 132ms"), args = list( "humanVerse.today", 5, TRUE, windows = TRUE, time = list( tz = "America/New_York", start = 1662477836.51212, end = 1662477841.07391),  cmd = "ping -n 5 humanVerse.today"));

	## args = .%$$% "res@args"
	info = NULL;
	info$cmd = list("cmd" = args$cmd, "domain" = args[[1]], "pings" = args[[2]], "internal" = args[[3]]);
	info$timer = list("tz" = args$time$tz, "start" = args$time$start, "stop" = args$time$end, "time.secs" = (args$time$end - args$time$start) );
	 
	result = NULL;
	
	lines = as.character(res);
	# may need to compile a list of ERRORs
	if(str.contains("Ping request could not find", lines[1]))
		{
cat("\n\n", lines[1], "\n\n");
		return(invisible(info));
		}
	
	
	
	what = "pinging";
	for(line in lines)
		{
		line = str.trim(line);
		if(line == "") { next; }
		if(what == "pinging")
			{
			if(str.contains("Pinging", line))
				{
				IP = str.between(line, keys=c("[", "]"));
				bytes = str.trim(str.between(line, keys=c("with", "bytes")));
				
				result$IP = IP;
				result$bytes = as.numeric(bytes);
				result$data = NULL;
				
				what = "reply";
				next;
				}
			}
		if(what == "reply")
			{
			if(str.contains("Reply from", line))
				{
				IP = str.between(line, keys=c("Reply from ", ":"));
				bytes = str.trim(str.between(line, keys=c("bytes=", " ")));
				## 'ASS-U-ME' milliseconds
				time = str.trim(str.between(line, keys=c("time=", "ms")));
				TTL = str.trim(str.between(line, keys=c("TTL=", "")));
				
				
				what = "reply";
				row = c(IP, bytes, time, TTL);
				result$data = rbind(result$data, row);
				next;
				} else {
						if(str.contains("Ping statistics", line))
							{
							next;  # nothing NEW in this data 
							}
						if(str.contains("Packets: Sent", line))
							{
							n = str.trim(str.between(line, keys=c("Sent = ", ",")));
							s = str.trim(str.between(line, keys=c("Received = ", ",")));
							
							result$bernoulli = list("n" = as.numeric(n), "s" = as.numeric(s), "f" = (as.numeric(n) - as.numeric(s)) );
							
							break;  # we have nothing more interesting in the data ... we can do summary ourselves 
							# seconds is still a string ... maybe not always "ms" ?
							}
						
						
						}
			}
		}
		# https://linux.die.net/man/8/ping
		# does 'ping' have a timer feature, ASSUME ms 
		result$data = as.data.frame(result$data);
			rownames(result$data) = NULL;
			colnames(result$data) = c("IP", "bytes", "time.ms", "TTL");
			# will IP hit a proxy and CHANGE if balance-loaded SERVER?
			# if not, I can remove it from the data frame 
				types = c("character", "numeric", "numeric", "numeric");
		result$data = df.setColumnType(result$data, ALL, types);
		# IP address
		# bytes 
		# rec ... bytes, ms, TTL
		# sum ... sent, success, failure ... 
	info$result = result;	
	return(invisible(info));
	}




















# see package pingr ... port options
# https://stackoverflow.com/questions/7012796/ping-a-website-in-r
## windows ... ping -n 5 google.com
## debian ... ping -c 5 google.com
# ping.domain = function(domain.com="humanVerse.today", count = 5, stderr = FALSE, stdout = "", ...)
	# {
	# # do we need a -n on NOT windows ...
	# flag = "c"; if(is.windows()) { flag = "n"; }
	# cmd = paste0("ping -", flag, " ",count, " ", domain.com);
	# cat("\n\n\n\t\t", cmd, "\n\n\n");
    # cli = system2(cmd, stderr = stderr, stdout = stdout, ...);
    # if (cli == 0) { TRUE } else { FALSE }
	# }



# ping <- function(x, stderr = "", stdout = "", ...){
    # pingvec <- system2("ping", x,
                       # stderr = stderr,
                       # stdout = stdout,...)
    # if (pingvec == 0) TRUE else FALSE
# }


# ping.domain = function(domain.com="humanVerse.today", count=5, intern = TRUE, ...)
	# {
	# args = function.arguments("main");
	# args = functions.getParameterInfo("main");
	# # do we need a -n on NOT windows ...
	# flag = "c"; if(is.windows()) { flag = "n"; args$windows = TRUE; }
	# cmd = paste0("ping -", flag, " ",count, " ", domain.com);
	# args$cmd = cmd;
	# cat("\n\n\n\t\t", cmd, "\n\n\n");
	# res = system( cmd , intern = intern, ...);
	# res = property.set("args", res, unlist(args));
	# res;
	# }



# ping.domain("8.8.8.8", 1)
# 

# WINDOZE
# (x = system("ping -n 5 google.com", intern = TRUE, show.output.on.console=TRUE))
# DEBIAN
# (x = system("ping -c 5 google.com", intern = TRUE))
# /usr/lib/firefox-esr/firefox-esr %u
# system(paste('"/usr/lib/firefox-esr/firefox-esr"', '-url cran.r-project.org'), wait = FALSE);

# open browser
# system(paste('"c:/Program Files/Mozilla Firefox/firefox.exe"', '-url cran.r-project.org'), wait = FALSE);
# C:\Program Files (x86)\Google\Chrome\Application\chrome.exe 
## SCAN hard drive ... looking for chrome.exe ... 

# (x = system("ping -n 5 google.com", intern = TRUE, show.output.on.console=TRUE))
# system2("ping", paste0("-c1 ","google.com")