
# see package pingr ... port options
# https://stackoverflow.com/questions/7012796/ping-a-website-in-r
## windows ... ping -n 5 google.com
## debian ... ping -c 5 google.com
ping.domain = function(domain.com, count = 5, stderr = FALSE, stdout = "", ...)
	{
	# do we need a -n on NOT windows ...
	flag = "c"; if(is.windows()) { flag = "n"; }
	cmd = paste0("ping -", flag, " ",count, " ", domain.com);
	cat("\n\n\n\t\t", cmd, "\n\n\n");
    cli = system2(cmd, stderr = stderr, stdout = stdout, ...);
    if (cli == 0) { TRUE } else { FALSE }
	}



ping <- function(x, stderr = "", stdout = "", ...){
    pingvec <- system2("ping", x,
                       stderr = stderr,
                       stdout = stdout,...)
    if (pingvec == 0) TRUE else FALSE
}


ping.domain = function(domain.com, count=5, intern = TRUE, ...)
	{
	args = functions.getParameterInfo("main");
	# do we need a -n on NOT windows ...
	flag = "c"; if(is.windows()) { flag = "n"; }
	cmd = paste0("ping -", flag, " ",count, " ", domain.com);
	cat("\n\n\n\t\t", cmd, "\n\n\n");
	res = system( cmd , intern = intern, ...);
	res = property.set("args", res, unlist(args));
	res;
	}

ping.parse = function(res)
	{
	
	
	
	}

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