
# https://linuxhandbook.com/ifconfig-debian/
# https://linuxhint.com/use-ip-command-debian-10-linux/
# ip link show

# debian appears similar
# > .b64_hex("vyPJNj")
# [1] "bf23c9363"
# > .b64_hex("zAl5gc")
# [1] "cc097981c"  ... 9876 ... 9857 ... cc0800080cc
# .hex_b64("cc0800080cc");  # "DMCAAIDM" ... DMCA ... MDIA 

.session = function()
	{
	# get current SESSION, unique id ...
	# appears that tempdir() is new with each SESSION 
	x = prep.path(tempdir(), trailing=FALSE);
	y = v.last(str.explode("/", x));
	z = v.last(str.explode("Rtmp", y));
	.b64_hex(z);  # 9 digits # "82d511633"
	}


windows.get = function(key = "users")
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(key);
	if(!ct.KEY || !is.character(key))	
		{ key = deparse(substitute(key)); } 
##########################################################
	KEY = prep.arg(key, n=4);


	WINDOZE = memory.get("-WINDOZE-", "-CACHE-");
	if(is.null(WINDOZE)) 
		{ 
		WINDOZE = windows.info();
		memory.set("-WINDOZE-", "-CACHE-", WINDOZE);
		}	
	
	x = WINDOZE;
	
	if(KEY %in% c("id", "iden", "info", "in", "syst", "sys"))
		{
		sn = x$data$bios$data[[1]]$SerialNumber;
		ci = x$data$cpu$data[[1]]$ProcessorId;		
		cn = x$data$cpu$data[[1]]$Name;
		z = c(sn, ci, cn);

		return( z );
		}
		
		
	if(KEY == "user")
		{ 
		y = x$data$useraccount$data;
		na = names(y[[1]]);
		
		# for(j in 1:length(na)) { .cat(j, " :: ", na[j]); print( unlist( list.getElements(y, j) ) ); }
			
		k = c("Name", "Disabled", "LocalAccount", "SID");
						
		n = length(k);
		z = NULL;
		for(i in 1:n)
			{
			idx = v.which(na, k[i]);
			col = unlist( list.getElements(y, idx) );
			if(k[i] == "SID")
				{
				# truncated / obfuscated ... a bit 
				col = paste0("... -", 
						list.getLastElements( 
							str.explode("-", col) ) );
				}
			z 	= cbind(z, col);
			}
		z = as.dataframe( z );
		colnames(z) = k;
		rownames(z) = 1:length(y);
		
		return( z );
		
		}
	if(KEY == "mac")
		{ 
		y = x$data$nic$data;
		na = names(y[[1]]);

		# k = c(15, 20, 27, 28);
		k = c("MACAddress", "ServiceName", "Name", 		
									"NetConnectionID");
						
		n = length(k);
		z = NULL;
		for(i in 1:n)
			{
			idx = v.which(na, k[i]);
			col = unlist( list.getElements(y, idx) );
			z 	= cbind(z, col);
			}
		z = as.dataframe( z );
		colnames(z) = k;
		rownames(z) = 1:length(y);
		
		return( z );
		}
	if(KEY %in% c("os","oper") )
		{
		y = x$data$os$data;
		na = names(y[[i]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}		
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("OS.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
	if(KEY == "bios")
		{
		y = x$data$bios$data;
		na = names(y[[1]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}		
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("BIOS.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
	if(KEY == "cpu")
		{
		y = x$data$cpu$data;
		na = names(y[[i]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}		
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("CPU.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
	if(KEY == "mem" || KEY == "memo")
		{
		y = x$data$memorychip$data;
		na = names(y[[i]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}		
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("RAM.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
	if(KEY %in% c("nic", "net", "netw"))
		{
		y = x$data$nic$data;
		na = names(y[[i]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}		
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("NIC.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
	if(KEY %in% c("logi", "log", "part", "par"))
		{
		# partitions 
		y = x$data$logicaldisk$data;
		na = names(y[[i]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("PARTITION.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
	
if(KEY %in% c("phys", "phy", "dis", "disc", "disk", "driv", "dri"))
		{
		# drives 
		y = x$data$diskdrive$data;
		na = names(y[[i]]);
		n = length(y);
		z = NULL;
		for(i in 1:n)
			{
			z = cbind(z, as.character(y[[i]]));
			}
		z = as.dataframe( cbind(na, z) );
		colnames(z) = c("Key", paste0("DISK.", 0:(n-1)))
		rownames(z) = 1:length(na);
		return(z);
		}
		
	return(x);
	}

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
# sudo dmidecode -t system | grep Serial ... debian = 0 on virtualbox 
# https://kb.mit.edu/confluence/pages/viewpage.action?pageId=152578725
# cat /proc/cpuinfo  ## virtual box has name ... 
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
# system("wmic diskdrive list", intern=TRUE);


## https://research.nccgroup.com/2022/03/10/microsoft-announces-the-wmic-command-is-being-retired-long-live-powershell/
## retiring, exploitable, powershell ... 

## 32 vs 64 bit 
## https://stackoverflow.com/questions/31510432/running-a-powershell-script-from-r-using-system2-rather-than-system
## POWERSHELL ... 

### 10/5/2022 1:50 PM ... 30 dips in the GANGA ... 5/9 = 0.555...

## https://www.improvescripting.com/how-to-get-disk-size-and-disk-free-space-using-powershell/

## Get-ComputerInfo 
## Get-CimInstance -Class CIM_PhysicalMemory 
## Get-CimInstance -Class CIM_LogicalDisk
## Get-TimeZone
## Get-TimeZone -ListAvailable
## Get-CimClass ... list all 
## ... htop ... Get-CimInstance -ClassName Win32_Process
## Get-CimClass -ClassName *Physi*
## https://superuser.com/questions/176624/linux-top-command-for-windows-powershell 
## Get-CimInstance -ClassName CIM_StatisticalInformation ... TMI 
## Get-CimInstance -ClassName Win32_PhysicalMemoryLocation ... NMI
## Get-CimInstance -ClassName CIM_InstalledOS ... NMI 
## Get-CimInstance -ClassName CIM_DiskPartition ... no drive letter
## Get-CimInstance -ClassName CIM_NetworkAdapter ... no mac address
## (Get-CimInstance -ClassName CIM_OperatingSystem).InstallDate
## ## (Get-CimInstance -ClassName CIM_OperatingSystem).SystemDrive
## how to gett all 
## Get-CimInstance -ClassName CIM_OperatingSystem | Select-Object BootDevice, BuildNumber, BuildType, Caption


# system2(command="ls", 
        # args=c("-l", "/etc/"), 
        # stdout="/tmp/stdout.log", 
        # stderr="/tmp/stderr.log", 
        # wait=TRUE)




## ## https://learn.microsoft.com/en-us/powershell/module/cimcmdlets/get-ciminstance?view=powershell-7.2
## Specifies the name of the CIM class for which to retrieve the CIM instances. You can use tab completion to browse the list of classes, because PowerShell gets a list of classes from the local WMI server to provide a list of class names.



# RAM = list("formfactor" = list("0" =  "Unknown", "1" =  "Other", "2" =  "SIP", "3" =  "DIP", "4" =  "ZIP", "5" =  "SOJ", "6" =  "Proprietary", "7" =  "SIMM", "8" =  "DIMM", "9" =  "TSOP", "10" =  "PGA", "11" =  "RIMM", "12" =  "SODIMM", "13" =  "SRIMM", "14" =  "SMD", "15" =  "SSMP", "16" =  "QFP", "17" =  "TQFP", "18" =  "SOIC", "19" =  "LCC", "20" =  "PLCC", "21" =  "BGA", "22" =  "FPBGA", "23" =  "LGA", "24" =  "FB-DIMM"), "memorytype" = list("0" = "Unknown", "1" = "Other", "2" = "DRAM", "3" = "Synchronous DRAM", "4" = "Cache DRAM", "5" = "EDO", "6" = "EDRAM", "7" = "VRAM", "8" = "SRAM", "9" = "RAM", "10" = "ROM", "11" = "Flash", "12" = "EEPROM", "13" = "FEPROM", "14" = "EPROM", "15" = "CDRAM", "16" = "3DRAM", "17" = "SDRAM", "18" = "SGRAM", "19" = "RDRAM", "20" = "DDR", "21" = "DDR2", "22" = "DDR2 FB-DIMM", "24" = "DDR3", "25" = "FBD2", "26" = "DDR4"));

# RAM = list("memorytype" = list(names = as.character(unlist(x$memorytype)), codes = as.integer(names(x$memorytype))), "formfactor" = list(names = as.character(unlist(x$formfactor)), codes = as.integer(names(x$formfactor))));

# RAM = list(memorytype = list(names = c("Unknown", "Other", "DRAM", "Synchronous DRAM", "Cache DRAM", "EDO", "EDRAM", "VRAM", "SRAM", "RAM", "ROM", "Flash", "EEPROM", "FEPROM", "EPROM", "CDRAM", "3DRAM", "SDRAM", "SGRAM", "RDRAM", "DDR", "DDR2", "DDR2 FB-DIMM", "DDR3", "FBD2", "DDR4"), codes = c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 24L, 25L, 26L)), formfactor = list(names = c("Unknown", "Other", "SIP", "DIP", "ZIP", "SOJ", "Proprietary", "SIMM", "DIMM", "TSOP", "PGA", "RIMM", "SODIMM", "SRIMM", "SMD", "SSMP", "QFP", "TQFP", "SOIC", "LCC", "PLCC", "BGA", "FPBGA", "LGA", "FB-DIMM"), codes = 0:24))



# https://winaero.com/get-cpu-information-via-command-prompt-in-windows-10/
#  wmic cpu get caption, deviceid, name, numberofcores, maxclockspeed, status
#  wmic diskdrive get model,serialNumber,size,mediaType
# wmic diskdrive list full
# wmic logicaldisk get name
# wmic logicaldisk list full

# cpu = system("wmic cpu", intern=);
# https://stackoverflow.com/questions/31510432/running-a-powershell-script-from-r-using-system2-rather-than-system
# system2("powershell", args=c("-file", "C:\\directoryName\\coolScript.ps1"))



# > quick(cat)
# "cat"

 # QUICK:  functions-cat.R  with idx:  7 
# NULL
# > quick(ping)
# "ping"

 # QUICK:  functions-ping.R  with idx:  63 
# NULL

suggest.username = function(choices=NULL, option=5)
	{
	option = as.integer(option);
	if(is.null(choices)) { choices = windows.get("users")$Name; }
	
	choices = str.removeWhiteSpace(choices);  # all but one 
		## debian like, no dots ... 
		option2 = gsub(REGEX_ALPHA_NUMERIC_NOT, "", choices);
		option1 = tolower(option2);
		
		info = str.explode(" ", choices);
		first = gsub(REGEX_ALPHA_NUMERIC_NOT, "", 
						list.getElements(info, 1) );
		last =  gsub(REGEX_ALPHA_NUMERIC_NOT, "",
						list.getLastElements(info) );
		n = length(first);
		option4 = option6 = option8 = option0 = character(n);
		for(i in 1:n)
			{
			F = first[i]; 
			L = last[i]; 
			option4[i] = paste0(F, charAt(L, 1)); 	# MonteS 
				if(F == L) { option4[i] = F; }
			option6[i] = paste0(charAt(F, 1), L); 	# MShaffer
				if(F == L) { option6[i] = L; }
			option8[i] = F;							# Monte 
			option0[i] = L;							# Shaffer 
			}
		option3 = tolower(option4);
		option5 = tolower(option6);
		option7 = tolower(option8);
		option9 = tolower(option0);
		
		
	options = list(option1, option2, 
						option3, option4, 
							option5, option6,
								option7, option8,
									option9, option0);
	
	x = choices; # cleansed
	# y = option5; # preffered ... preferred 
	if(option == 0) { option = 10; }
	y = options[[option]];
	names(y) = x;
	y = property.set("options", y, options);
	y;
	}


which.computer = function(use.cache = FALSE)
	{
	if(is.windows())
		{
		info = windows.get("id");
		}
	
	## debian ...
	# USER                    mshaffer
	# R_SESSION_TMPDIR        /tmp/RtmpvyPJNj

	# "sudo" ... 
	# PWD                     /home/mshaffer
	# R_SESSION_TMPDIR        /tmp/RtmpzAl5gc
	# SUDO_USER               mshaffer
	
	
	## windows ...
	# # HOME                                      C:\Users\Monte J. Shaffer\Documents
# HOMEDRIVE                                 C:
# HOMEPATH                                  \Users\Monte J. Shaffer

# USERNAME                                  Monte J. Shaffer
# USERPROFILE                               C:\Users\Monte J. Shaffer
# VBOX_MSI_INSTALL_PATH                     C:\Program Files\Oracle\VirtualBox\  
## "C:\\Users\\MONTEJ~1.SHA\\AppData\\Local\\Temp\\Rtmp gtURYz"
## new window ... that is the SESSION_ID ... 
## "C:\\Users\\MONTEJ~1.SHA\\AppData\\Local\\Temp\\Rtmp mYFMU5"
## > bs = "RtmpgtURYz"
# > .b64_hex(bs)
# [1] "46d9a982d511633"
# > bs = "gtURYz"
# > .b64_hex(bs)
# [1] "82d511633"
# > strlen( .b64_hex(bs) )
# [1] 9
# > osVersion
## [1] "Windows 10 x64 (build 19044)"
##
# ## R
# > R.version  ## svn ID ... 
               # _                                
# platform       x86_64-w64-mingw32               
# arch           x86_64                           
# os             mingw32                          
# crt            ucrt                             
# system         x86_64, mingw32                  
# status                                          
# major          4                                
# minor          2.1                              
# year           2022                             
# month          06                               
# day            23                               
# svn rev        82513                            
# language       R                                
# version.string R version 4.2.1 (2022-06-23 ucrt)
# nickname       Funny-Looking Kid   

## 10/05/2022 ... 200/360 days?  5/9 ... march 21 start - ish 
## times (4ths/12ths) and seasons (3rds/9ths)
## cats and dogs vs cats are dogs ...
## 40 + 40 + 40 ... three times ... 360 
## 30 + 30 + 30 ... four times ... 360 
## 5/9 .... 0.555555555555555555555555555555555555555555
	# 
	# wmic bios get serialnumber
	# wmic nic 	get 
	
	## https://stackoverflow.com/questions/7096989/how-to-save-all-console-output-to-file-in-r
	
	# > sink("sink-examp.txt")
# > i <- 1:10
# > outer(i, i)
# > sink()
# > x = readTextFile("sink-examp.txt")
	
	#https://stackoverflow.com/questions/5045450/how-to-get-cpu-serial-under-linux-without-root-permissions
	
	## res = system('dmidecode | grep -w UUID | sed "s/^.UUID\: //g"', intern=TRUE); 
	## sudo R ... 
	## res = system('dmidecode | grep -w UUID | sed "s/^.UUID\\: //g"', intern=TRUE); 
	# # UUID ... 
	# # root@debian:/home/mshaffer# dmidecode | grep -w UUID | sed "s/^.UUID\: //g"
# bb14587d-0a12-d246-9350-95232fcc7515

# https://stackoverflow.com/a/22991546/184614

# root@debian:/home/mshaffer# dmesg | grep UUID | grep "Kernel" | sed "s/.*UUID=//g" | sed "s/\ ro\ quiet.*//g"
# e168a6b1-920f-4c95-847f-72df06bdc42a

	
	# bio SerialNumber         : chr "6VKSMX1"
	# cpu ProcessorId          : chr "BFEBFBFF000306C3"
	# first disk ... $ PNPDeviceID                : chr "SCSI\\DISK&amp;VEN_SAMSUNG&amp;PROD_SSD\\4&amp;395BA125&amp;0&amp;000000"
	# logical c: partition $ VolumeSerialNumber          : chr "6AE937B1"
	# ram serial numbers ... $ SerialNumber        : chr "A9CCC594"


	
	}

windows.info = function(use.cache = TRUE, intern = TRUE, ...)
	{
	# cache to file ... 
	f = "C:/_R_/-humanVerse-/SYSTEM/cache/windows.info.rds";
	if(use.cache && file.exists_(f)) { return(readRDS(f));}
	   
	
	todos = c("bios", "cpu", "diskdrive", "logicaldisk", "memorychip", "nic", "os", "useraccount"); 
		# product is software installed ?
		# 
	RES = common.args();
	RES$data = list();
	for(todo in todos)
		{
		args = common.args();
		cmd = paste0("wmic ",todo," list /format:csv");	
		if(todo == "os")
			{
			# other simpler form doesn't work ...
			cmd = "wmic os get BootDevice,BuildNumber,BuildType,Caption,CodeSet,CountryCode,CreationClassName,CSCreationClassName,CSDVersion,CSName,CurrentTimeZone,DataExecutionPrevention_32BitApplications,DataExecutionPrevention_Available,DataExecutionPrevention_Drivers,DataExecutionPrevention_SupportPolicy,Debug,Description,Distributed,EncryptionLevel,ForegroundApplicationBoost,FreePhysicalMemory,FreeSpaceInPagingFiles,FreeVirtualMemory,InstallDate,LargeSystemCache,LastBootUpTime,LocalDateTime,Locale,Manufacturer,MaxNumberOfProcesses,MaxProcessMemorySize,MUILanguages,Name,NumberOfLicensedUsers,NumberOfProcesses,NumberOfUsers,OperatingSystemSKU,Organization,OSArchitecture,OSLanguage,OSProductSuite,OSType,OtherTypeDescription,PAEEnabled,PlusProductID,PlusVersionNumber,PortableOperatingSystem,Primary,ProductType,RegisteredUser,SerialNumber,ServicePackMajorVersion,ServicePackMinorVersion,SizeStoredInPagingFiles,Status,SuiteMask,SystemDevice,SystemDirectory,SystemDrive,TotalSwapSpaceSize,TotalVirtualMemorySize,TotalVisibleMemorySize,Version,WindowsDirectory  /format:csv";

			}
		args$cmd 	= cmd;
.cat("\n\t\t windows.info :: ", cmd, "\n"); flush.console();
		res = system( cmd , intern = intern);		
		#	res = system( cmd , intern = intern, ...);

		args = common.stop(args);
		
		lines 		= as.character(res);
		lines 	= v.TO( str.trim(lines), "", NULL);
		args$raw 	= lines;
	
		## PARSING 		
		k 		= str.explode(COMMA, lines[1]);
		
		n = length(lines);
		for(i in 2:n)
			{
			# multiples ... RAM, harddrives, CPUs?
			v		= str.explode(COMMA, lines[i]);
			info 	= list.create(k,v);
			args$data[[i-1]] = info;
			}

		log.cmd( paste0("wmic-"), 
					paste0(todo, "_", args$uniqid), args);

		RES$data[[todo]] = args;
		}
		
		
		RES = common.stop(RES);		
		log.cmd(paste0("windows.info"), RES$uniqid, RES);
		
		# two copies ... 
		f = "C:/_R_/-humanVerse-/SYSTEM/cache/windows.info.rds";
		f = prep.path(f); check.dir(f);
			writeRDS(RES, f);
		b = paste0("C:/_R_/-humanVerse-/SYSTEM/cache/-history-/windows.info_",RES$uniqid,".rds");
		b = prep.path(b); check.dir(b);
			writeRDS(RES, b);
		 
		return(RES);
		
		stop("monte");
	
	### sink(aldsj.txt) ... code ... sink() ... easy ...
	### how to pipe code to R ... even if I have script files ...
	### maybe have a while(1) loop ... listening for a new TODO file ... TODO_uniqid .txt ... when complete, move it to /log/input ... the sink(uniqid) ... code ... goes to /log/output ... maybe one file ... by date, session ... 
	
	##  TERM                    xterm-256color
	## COLORTERM               truecolor
# https://cran.r-project.org/web/packages/startup/vignettes/startup-intro.html

# buildPotentialUsernames("Monte J. Shaffer");
# mshaffer montejshaffer montes monteshaffer 
	
	# 
	
	
# wmic bio 
# is this "language separator", different per system 
# what do they do with "," in the key/val ? SKARY
#	cmd = paste0("wmic cpu list /format:csv");	
#	cmd = paste0("wmic cpu");	
	cmd = paste0("wmic cpu list /format:csv");	
	# what if you have TWO physical CPU's
	##	... PYTHON PARSER machine?


# \s{2,}

	args$cmd 	= cmd;

	.cat("\n\t\t windows.info :: ", cmd, "\n"); flush.console();

	res = system( cmd , intern = intern);
#	res = system( cmd , intern = intern, ...);

	lines 		= as.character(res);
	args$raw 	= lines;
	
	
	lines 		= v.TO( str.trim(lines), "", NULL);
	
	k = str.explode(COMMA, lines[1]);
	v = str.explode(COMMA, lines[2]);

	
	kidx = cumsum(klen) + 2 * (1:nk);
	
	# info = strsplit(lines, "[[:space:]]{2,}");
	
	
	# https://stackoverflow.com/a/72032990/184614
	log.cmd("wmic/cpu", args$uniqid, args);

	# seems to be fixed width ... csv ... pipe 

dput.one(res);	
	
	stop("monte");
	
	log.cmd("wmic/cpu", args$uniqid, args);
	
	
	
	
	
	
	}
 

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

common.stop = function() {}	
common.stop = function(args = list() )
	{	
	args$timer$ne = .now();
	args$timer$rt = args$timer$ne - args$timer$ns;
	args;
	}

common.args = function() {}	
common.args = function(args = list() )
	{
	args$uniqid 		= .uniqid();
	args$timestamp 		= .timestamp("full");
	args$timer$ns		= .now();
	
	args;
	}


ping.domain = function(domain = "humanVerse.today", 
							n = 5, intern = TRUE, ... )
	{   
 
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.DOMAIN = check.type(domain);
	if(!ct.DOMAIN || !is.character(domain))	
		{ domain = deparse(substitute(domain)); } 
##########################################################


	args = common.args();
	
	args$domain	= domain;
	args$n		= n;
	
	flag = "c";  args$windows = FALSE;
	if(is.windows()) { flag = "n"; args$windows = TRUE; }
	args$flag 	= flag;

	## windows ... ping -n 5 google.com
	## debian ... ping -c 5 google.com	
	cmd = paste0("ping -", flag, " ",n, " ", domain);	
	args$cmd 	= cmd;
	
.cat("\n\t\t ping.domain :: ", cmd, "\n"); flush.console();

	res = system( cmd , intern = intern, ...);
	
	args = common.stop(args);

	res = property.set("args", res, args);
# dput.one(res);
	ping.parse(res);
	}  

 
# > str(res)
 # chr [1:12] "" "Pinging humanVerse.today [66.29.141.54] with 32 bytes of data:" "Reply from 66.29.141.54: bytes=32 time=126ms TTL=48" ...
 # - attr(*, "args")=List of 8
  # ..$ uniqid   : chr "1664983942.903851.8b869"
  # ..$ timestamp: chr "2022-10-05 15:32:22 +0000"
  # ..$ domain   : chr "humanVerse.today"
  # ..$ count    : num 5
  # ..$ windows  : logi TRUE
  # ..$ flag     : chr "n"
  # ..$ cmd      : chr "ping -n 5 humanVerse.today"
  # ..$ timer    :List of 3
  # .. ..$ ne: num 1.66e+09
  # .. ..$ ns: num 1.66e+09
  # .. ..$ rt: num 4.46



## x = ping.domain(n=22)
# > str(x)
# List of 11
 # $ uniqid   : chr "1664990793.279787.8778b"
 # $ timestamp: chr "2022-10-05 17:26:33 +0000"
 # $ timer    :List of 3
  # ..$ ns: num 1.66e+09
  # ..$ ne: num 1.66e+09
  # ..$ rt: num 21.5
 # $ domain   : chr "humanVerse.today"
 # $ n        : num 22
 # $ windows  : logi TRUE
 # $ flag     : chr "n"
 # $ cmd      : chr "ping -n 22 humanVerse.today"
 # $ lines    : chr [1:29] "" "Pinging humanVerse.today [66.29.141.54] with 32 bytes of data:" "Reply from 66.29.141.54: bytes=32 time=82ms TTL=48" "Reply from 66.29.141.54: bytes=32 time=81ms TTL=48" ...
 # $ raw      : chr "\nPinging humanVerse.today [66.29.141.54] with 32 bytes of data:\nReply from 66.29.141.54: bytes=32 time=82ms T"| __truncated__
 # $ data     :List of 3
  # ..$ pinging  :List of 2
  # .. ..$ IP   : chr "66.29.141.54"
  # .. ..$ bytes: int 32
  # ..$ reply    :'data.frame':   22 obs. of  4 variables:
  # .. ..$ IP   : chr [1:22] "66.29.141.54" "66.29.141.54" "66.29.141.54" "66.29.141.54" ...
  # .. ..$ bytes: int [1:22] 32 32 32 32 32 32 32 32 32 32 ...
  # .. ..$ time : int [1:22] 82 81 81 80 84 81 82 81 82 80 ...
  # .. ..$ TTL  : int [1:22] 48 48 48 48 48 48 48 48 48 48 ...
  # ..$ bernoulli:List of 3
  # .. ..$ n: int 22
  # .. ..$ s: int 22
  # .. ..$ f: int 0


#dput(res);
	# res = structure(c("", "Pinging humanVerse.today [66.29.141.54] with 32 bytes of data:", "Reply from 66.29.141.54: bytes=32 time=126ms TTL=48", "Reply from 66.29.141.54: bytes=32 time=94ms TTL=48", "Reply from 66.29.141.54: bytes=32 time=145ms TTL=48", "Reply from 66.29.141.54: bytes=32 time=82ms TTL=48", "Reply from 66.29.141.54: bytes=32 time=84ms TTL=48", "", "Ping statistics for 66.29.141.54:", "    Packets: Sent = 5, Received = 5, Lost = 0 (0% loss),", "Approximate round trip times in milli-seconds:", "    Minimum = 82ms, Maximum = 145ms, Average = 106ms"), args = list(    uniqid = "1664983942.903851.8b869", timestamp = "2022-10-05 15:32:22 +0000",     domain = "humanVerse.today", count = 5, windows = TRUE, flag = "n",     cmd = "ping -n 5 humanVerse.today", timer = list(ne = 1664983947.36945,         ns = 1664983942.90808, rt = 4.46136999130249)))
  

nic.info = function() {}
# ipconfig ifconfig ... ip instead ... 
# ip -a -h link 
## traceroute tracert ...
tracert = function() {}

ping.parse = function(res)
	{
	# ?args ... not javascript 'arguments'
	# args = .%$$% "res@args"
	args = property.get("args", res);  
	
	lines = as.character(res);	
#	raw = paste0(lines, collapse="\n");
#	args$lines 	= lines;
#	args$raw 	= raw;
	args$raw 	= lines;

	
#.cat(raw);
.cat(lines, psep="\n");

	data = NULL;
	
.___windows_parser = function() {}
	if(is.windows())
		{
		# may need to compile a list of ERRORs
		if(.anyTRUE(
			str.contains("Ping request could not find", lines)) )
			{
			log.cmd("ping", args$uniqid, args);
			# good choice for minvisible (can't get it BACK)
			return( invisible(args) );
			}
			
		# https://linux.die.net/man/8/ping
		# does 'ping' have a timer feature, ASSUME ms 
			
		# walk the lines ... 
		data$reply = NULL;
		for(line in lines)
			{
			line = str.trim(line);	if(line == "") { next; }
			if(str.contains("Pinging", line))
				{ 
				# line = 'Pinging 8.8.8.8 with 32 bytes of data:'
				# line = 'Pinging humanVerse.today [66.29.141.54] with 32 bytes of data:'

				D 	= str.trim(str.between("Pinging", line, "with"));
				IP 	= str.between("[", line, "]");
				if(is.na(IP)) { IP = D; }
					
				bytes = str.trim(str.between("with", line, "bytes"));
				bytes = as.integer(bytes);
				
				data$pinging = list(IP = IP, bytes = bytes);
				next;
				}
			
			if(str.contains("Reply from", line))
				{
				# IP is redundant, I don't believe there will 
				# be a load-server 'switch' on this few of packets 
				IP = str.between("Reply from ",line, ":");
				
				bytes = str.trim(str.between("bytes=", line, " "));
				bytes = as.integer(bytes);
				
				## 'ASS-U-ME' milliseconds
				
				time = str.trim(str.between("time=", line, "ms"));
				time = as.integer(time);
				 
				TTL = str.trim(str.between("TTL=", line, ""));
				TTL = as.integer(TTL);
				  
				row = df.row(IP, TTL, bytes, time, use.names=TRUE);
				data$reply = rbind(data$reply, row);
				next;
				}
				
				# nothing new here ... R and stats ...
				# str.contains("Ping statistics", line)
				
			if(str.contains("Packets: Sent", line))
				{
				nt = str.between("Sent = ", line, COMMA); 
				ns = str.between("Received = ", line, COMMA);
				
				nt = as.integer(nt);
				ns = as.integer(ns);
				nf = nt - ns;
				
				data$bernoulli = list(n = nt, s = ns, f = nf);
				
				# end of the lines ...
				break;
				}
			} # end of for

		args$data = data;
		log.cmd("ping", args$uniqid, args);
		return( invisible(args) );  # good choice for minvisible
		# end of windows ... end wendd0ze ... wd0ze 
		}
 		
.___debian_parser = function() {}		
	
	# res = system(" ping -c 5 google.com ", intern=TRUE);	
	# dput(res);
		# may need to compile a list of ERRORs
		if(.anyTRUE(
			str.contains("Name or service not known", lines)) )
			{
			log.cmd("ping", args$uniqid, args);
			# good choice for minvisible (can't get it BACK)
			return( invisible(args) );
			}
			
		# https://linux.die.net/man/8/ping
		# does 'ping' have a timer feature, ASSUME ms 
			
		# walk the lines ... 
		data$reply = NULL;
		for(line in lines)
			{
			line = str.trim(line);	if(line == "") { next; }
			if(str.contains("PING ", line))
				{ 
				# line = 'PING google.com (142.250.190.142) 56(84) bytes of data.'
				# line = 'PING 8.8.8.8 (8.8.8.8) 56(84) bytes of data.'

				D 	= str.trim(str.between("PING", line, "("));
				r 	= str.after(D, line);
				IP 	= str.between("(", r, ")");  # first one 
				r 	= str.after(IP, r);
				
				# A correctly formed ping packet is typically 56 bytes in size, or 64 bytes when the Internet Control Message Protocol (ICMP) header is considered, and 84 bytes including Internet Protocol (IP) version 4 header.
				
				bytesP = str.between("(", r, ")");  # first one	
				r 	= str.before(bytesP, r);
				
				bytesP = as.integer(bytesP);
				
				
				bytes = str.trim(str.replace(")","",r));
				bytes = as.integer(bytes);
				
				data$pinging = list(IP = IP, bytesP = bytesP, bytes = bytes);
				next;
				}
			
			if(str.contains("bytes from", line))
				{
				# line = '64 bytes from 8.8.8.8: icmp_seq=1 ttl=114 time=34.7 ms';
				# line = '64 bytes from ord38s32-in-f14.1e100.net (142.250.191.238): icmp_seq=2 ttl=55 time=34.3 ms';
				# ord38s32-in-f14.1e100.net
				# https://dnslytics.com/ip/172.217.6.14
				# https://dnslytics.com/ip/142.250.191.238
				

				H = str.between("bytes from ",line, ":");
				I = str.between("(", H, ")");
				if(!is.na(I)) 	
					{ 
					IP = I; 
					PTR = str.trim(str.before("(", H));
					} else { IP = H; PTR = ""; }
				
				bytes = str.trim(str.between("", line, " bytes"));
				bytes = as.integer(bytes);
				
				## 'ASS-U-ME' milliseconds
				
				time = str.trim(str.between("time=", line, "ms"));
				time = as.numeric(time);
				 
				TTL = str.trim(str.between("ttl=", line, " "));
				TTL = as.integer(TTL);
				
				ICMP = str.trim(str.between("icmp_seq=", line, " "));
				ICMP = as.integer(ICMP);
				  
				row = df.row(IP, PTR, ICMP, TTL, bytes, time, use.names=TRUE);
				data$reply = rbind(data$reply, row);
				next;
				}
				
				# nothing new here ... R and stats ...
				# str.contains("Ping statistics", line)
				
			# https://unix.stackexchange.com/a/332017/278016
			# get the ping statistics durning it's execution: Just press Ctrl + | (vertical slash or it's also called pipe line)
			# line = "5 packets transmitted, 5 received, 0% packet loss, time 9ms" ; #[ what is time 9ms]? latency?
			if(str.contains("packets transmitted", line))
				{
				nt = str.between("", line, " packets transmitted"); 
				ns = str.between(", ", line, " received");
				
				nt = as.integer(nt);
				ns = as.integer(ns);
				nf = nt - ns;
				
				data$bernoulli = list(n = nt, s = ns, f = nf);
				
				ti = str.between("time ", line, "ms");
				ti = as.integer(ti);
				date$time = ti;
				
				# end of the lines ...
				break;
				}
			} # end of for

		args$data = data;
		log.cmd("ping", args$uniqid, args);
		return( invisible(args) );  # good choice for minvisible
		
	
	}

  



















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







# WHY won't 7zip do this? ... 
# > ?untar
# > x = path.fromClipboard()
# > x
# [1] "C:/_git_/-R-/"
# > setwd(x)
# > untar("RCurl_1.98-1.8.tar.gz")



# https://daniel.haxx.se/blog/2021/09/27/common-mistakes-when-using-libcurl/
# # // Keep the URL as a C++ string object
# std::string str("https://example.com/");

# // Pass it to curl as a C string!
# curl_easy_setopt(curl, CURLOPT_URL, str.c_str());


# how many b64 
# RCurl ...
# .Call(R_base64_encode, txt, asRaw)
# where is the internal function?

# htmlTreeParse?
# where are all these functions???
# S? functions 
# https://www.rfc-editor.org/rfc/rfc9110.html#name-status-codes
# you could LINK to this page with anchor ... # https://www.rfc-editor.org/rfc/rfc9110.html#name-205-reset-content
#  
# KAGGLE ... urban dictionary 
# https://www.kaggle.com/datasets/therohk/urban-dictionary-words-dataset
# https://www.kaggle.com/datasets/rizimore/brainyquote-topics
