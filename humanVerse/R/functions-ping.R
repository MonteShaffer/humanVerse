# WINDOZE
# mac_addr<- system("getmac", intern = TRUE)
# DEBIAN 
# mac_addr<- system("ifconfig -a | grep -Po 'HWaddr \K.*$'", intern = TRUE)
# // ifconfig, ip, or ipconfig
# system("ipconfig /all", intern = TRUE)
# system("wmic bios", intern=TRUE);
# wmic bios
# wmic bios get serialnumber  ... windoze
# ioreg -l | grep IOPlatformSerialNumber ... mac 
# sudo dmidecode -t system | grep Serial ... debian 
# https://kb.mit.edu/confluence/pages/viewpage.action?pageId=152578725
# cat /proc/cpuinfo
# cat /proc/meminfo

# MAC ... system_profiler SPSoftwareDataType SPHardwareDataType
# https://osxdaily.com/2012/04/28/get-extended-cpu-information-from-the-command-line/
# https://osxdaily.com/2022/02/02/find-mac-system-info-terminal-system-profiler/
# RAM ... wmic memorychip get devicelocator, manufacturer
# wmic memorychip get devicelocator, partnumber
# wmic memorychip get devicelocator, partnumber, manufacturer,banklabel, serialnumber, capacity, speed 
# https://www.windowscentral.com/how-get-full-memory-specs-speed-size-type-part-number-form-factor-windows-10
# system("wmic memorychip get devicelocator, partnumber, manufacturer,banklabel, serialnumber, capacity, speed, memorytype, formfactor", intern=TRUE);

# system("wmic logicaldisk list full", intern=TRUE);
# system("wmic physicaldisk list full", intern=TRUE);


# RAM = list("formfactor" = list("0" =  "Unknown", "1" =  "Other", "2" =  "SIP", "3" =  "DIP", "4" =  "ZIP", "5" =  "SOJ", "6" =  "Proprietary", "7" =  "SIMM", "8" =  "DIMM", "9" =  "TSOP", "10" =  "PGA", "11" =  "RIMM", "12" =  "SODIMM", "13" =  "SRIMM", "14" =  "SMD", "15" =  "SSMP", "16" =  "QFP", "17" =  "TQFP", "18" =  "SOIC", "19" =  "LCC", "20" =  "PLCC", "21" =  "BGA", "22" =  "FPBGA", "23" =  "LGA", "24" =  "FB-DIMM"), "memorytype" = list("0" = "Unknown", "1" = "Other", "2" = "DRAM", "3" = "Synchronous DRAM", "4" = "Cache DRAM", "5" = "EDO", "6" = "EDRAM", "7" = "VRAM", "8" = "SRAM", "9" = "RAM", "10" = "ROM", "11" = "Flash", "12" = "EEPROM", "13" = "FEPROM", "14" = "EPROM", "15" = "CDRAM", "16" = "3DRAM", "17" = "SDRAM", "18" = "SGRAM", "19" = "RDRAM", "20" = "DDR", "21" = "DDR2", "22" = "DDR2 FB-DIMM", "24" = "DDR3", "25" = "FBD2", "26" = "DDR4"));

# RAM = list("memorytype" = list(names = as.character(unlist(x$memorytype)), codes = as.integer(names(x$memorytype))), "formfactor" = list(names = as.character(unlist(x$formfactor)), codes = as.integer(names(x$formfactor))));

# RAM = list(memorytype = list(names = c("Unknown", "Other", "DRAM", "Synchronous DRAM", "Cache DRAM", "EDO", "EDRAM", "VRAM", "SRAM", "RAM", "ROM", "Flash", "EEPROM", "FEPROM", "EPROM", "CDRAM", "3DRAM", "SDRAM", "SGRAM", "RDRAM", "DDR", "DDR2", "DDR2 FB-DIMM", "DDR3", "FBD2", "DDR4"), codes = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 24L, 25L, 26L)), formfactor = list(names = c("Unknown", "Other", "SIP", "DIP", "ZIP", "SOJ", "Proprietary", "SIMM", "DIMM", "TSOP", "PGA", "RIMM", "SODIMM", "SRIMM", "SMD", "SSMP", "QFP", "TQFP", "SOIC", "LCC", "PLCC", "BGA", "FPBGA", "LGA", "FB-DIMM"), codes = 0:24))

# https://winaero.com/get-cpu-information-via-command-prompt-in-windows-10/
#  wmic cpu get caption, deviceid, name, numberofcores, maxclockspeed, status
#  wmic diskdrive get model,serialNumber,size,mediaType
# wmic diskdrive list full
# wmic logicaldisk get name
# wmic logicaldisk list full

# https://stackoverflow.com/questions/31510432/running-a-powershell-script-from-r-using-system2-rather-than-system
# system2("powershell", args=c("-file", "C:\\directoryName\\coolScript.ps1"))


# DeviceID         Size BusType MediaType model                     serialnumber
# --------         ---- ------- --------- -----                     ------------
# 0        500107862016 RAID    SSD       Samsung SSD 850 EVO 500GB S3R3NF1JB22176L
# $Drives=0 ; $TotalSize=0
# get-physicaldisk | 
  # ForEach {$_;$Drives+=1;$TotalSize+=$_.Size}|
    # Format-Table -auto DeviceID,Size,BusType,MediaType,model,serialnumber
# "Drives       TotalSize"
# "------- --------------"
# "{0,-6} {1,15}" -f $Drives,$TotalSize



check.internet = function(domain="8.8.8.8", count=1)
	{
	res = ping.domain(domain, count);
	# print(res$result$data); # already printed  
	minvisible(res);
	}
  
ping.domain = function(domain="humanVerse.today", count=5, 
							intern = TRUE, ...
						)
	{   
	args = function.arguments("main");
# dput(args); 
	flag = "c"; if(is.windows()) { flag = "n"; args$windows = TRUE; }
	args$time$tz = Sys.timezone();
	args$time$start = as.numeric(Sys.time());  # as.POSIXct , tz="UTC");  # UTC doesn't work   
	
	## windows ... ping -n 5 google.com
	## debian ... ping -c 5 google.com	
	cmd = paste0("ping -", flag, " ",count, " ", domain);	
	args$cmd = cmd;
	
cat("\n\n\n\t\t ping.domain :: ", cmd, "\n\n\n"); flush.console();
	res = system( cmd , intern = intern, ...);
	args$time$end = as.numeric(Sys.time());   # as.POSIXct tz="GMT");  # GMT doesn't work 
	res = property.set("args", res, args);
#dput(res);  # don't parse on NO DATA???
	ping.parse(res);
	}  

 

#dput(res);
	# res = structure(c("", "Pinging humanVerse.today [66.29.141.54] with 32 bytes of data:", "Reply from 66.29.141.54: bytes=32 time=93ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=90ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=199ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=90ms TTL=49", "Reply from 66.29.141.54: bytes=32 time=190ms TTL=49", "", "Ping statistics for 66.29.141.54:", "    Packets: Sent = 5, Received = 5, Lost = 0 (0% loss),", "Approximate round trip times in milli-seconds:",  "    Minimum = 90ms, Maximum = 199ms, Average = 132ms"), args = list( "humanVerse.today", 5, TRUE, windows = TRUE, time = list( tz = "America/New_York", start = 1662477836.51212, end = 1662477841.07391),  cmd = "ping -n 5 humanVerse.today"));


ping.parse = function(res)
	{
	# args = .%$$% "res@args"
	args = property.get("args", res);  
	info = NULL;
	info$cmd = list("cmd" = args$cmd, "domain" = args[[1]], "pings" = args[[2]], "internal" = args[[3]]);
	info$timer = list("tz" = args$time$tz, "start" = args$time$start, "stop" = args$time$end, "time.secs" = (args$time$end - args$time$start) );
	 
	result = NULL;
	
	lines = as.character(res);     
		info$raw = paste0("\n", paste0(lines, collapse="\n"), "\n\n");
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
				# doesn't work if IP is passed in 
				# IP = str.between(line, keys=c("[", "]"));
				IP = str.trim(str.between(line, keys=c("Pinging", "with")));
				IP = str.trim(str.replace( c("[", "]"), "", IP)); # in case was [IP]
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
	# print(result$data);
		ping.summary = result$data;
	pip(ping.summary, isolate.row=FALSE, col.width=22); 
	# return(invisible(info));  # with invisible() would be nice if there was one MEMORY elment in stack "get.last" ... I can make that happen with my own invisible wrapper ...  
	# minvisible(info);
	return(minvisible(info)); 
	}

  
ping = ping.domain;



















# see package pingr ... port options
# https://stackoverflow.com/questions/7012796/ping-a-website-in-r
## windows ... ping -n 5 google.com
## debian ... ping -c 5 google.com

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