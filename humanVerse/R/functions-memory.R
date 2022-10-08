

 
memory.init = function(purge.memory = FALSE, verbose = TRUE)
	{
	if((!exists(".humanVerse") || purge.memory) || (exists(".humanVerse") &&is.null(.humanVerse)))
		{
		.GlobalEnv$.humanVerse = list();
if(verbose)
	{
.cat("humanVerse::memory.init; ... initializing HIDDEN list '.humanVerse'");
	}		
		}
	return(TRUE);
	}








# getBASE64 key from Sys.getenv("humanVerse")
# path to file of config ... if exists, load config
# # memory.smartRestore();


system.reset = function(keep.humanVerse=TRUE)
	{
	gc();
	gcinfo(TRUE);
	
	BASE_PKGS = c("stats", "graphics", "grDevices", 
					"utils", "datasets", "methods", "base");
	
	HV_PKGS = c("humanVerse", "HVcpp");
	if(keep.humanVerse) { BASE_PKGS = c(BASE_PKGS, HV_PKGS); }
	
		{ 
		# .packages 
		# .rmpkg 
		s = search();
		ss = str.contains("package:", s);
		ns = s[ss];
		nss = str.explode("package:", ns);
		# we aren't going to have any of these functions ... AFTER ... 
		nsss = str.trim(list.getElements(nss,2));
		PKGS = nsss;
		d = v.return(set.diff(PKGS, BASE_PKGS));
		if(!is.null(d))
			{
			n = length(d);
			for(i in 1:n)
				{
				detach(d[i], character.only = TRUE, unload = TRUE);
				}
			}
		}
	
	
	# remove ".humanVerse"? 
		toRM = ls(all.names=TRUE, envir = .GlobalEnv);
		if(keep.humanVerse)
			{
			# save RDS to disk ... rm, then restore .humanVerse 
			}
		HV = .humanVerse;  # save to disk RDS ...  
	rm(list = toRM, envir = .GlobalEnv);
		.humanVerse = HV;	
		
	gcinfo(verbose = FALSE);
	gc(TRUE);
	gc(reset = TRUE);
	}


# maybe do an AUTO SAVE with hidden timer
# check every time memory.init() is called
memory.saveState = function() {}
memory.restoreState = function() {}
# AUTO-SAVE as a CONFIG ...
# parseINI





# params are properties
memory.start = function() {}
memory.start = function(	key="timer", 
							MEMORY="TIMER", 
							params = list("tz" = timer.tz()), 
							force.purge=FALSE 
						)
	{
	memory.log(key, MEMORY, "start");
	what = .GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]];
	if(is.null(what) || force.purge )
		{
		what = list();
		for(param in names(params))
			{
			what = property.set(param, what, params[[param]] );
			} 
		.GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]] = what;
		}	
	}

 

# set a log level ... 
# log to file ... 
memory.log = function(key, MEMORY, action="get")
	{
	return(NULL);  # TODO :: update this 
	# manual set the value so no recursion 
	# we are logging timestamps, not values ... 
		now = as.POSIXct(Sys.time());
		info = df.row(now, action, MEMORY, key, use.names=TRUE); # works nicely 
	what = .GlobalEnv$.humanVerse[["."]][["-SYSTEM_LOG-"]];
	if(is.null(what)) { what = info; } else { what = rbind(what,info); }
	.GlobalEnv$.humanVerse[["."]][["-SYSTEM_LOG-"]] = what;
	}
	
	
memory.clear = function(MEMORY = "-B64_HEX-")	
	{
	# deletes everyting in the MEMORY block or group 
	.GlobalEnv$.humanVerse[["."]][[MEMORY]] = NULL;
	}
	
	
# DOWN ARROW on CONSOLE BRINGS UP WORD 'ANS' ... how to ADD to history??? 

memory.get = function(key, MEMORY="BASE", unused=NULL) 
	{	
	memory.log(key, MEMORY, "get");
	.GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]];
	}


# now the same ... KEY on OBJ to VAL
memory.set = function(key, MEMORY="BASE", value) 
	{
	memory.log(key, MEMORY, "set");
	.GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]] = value;	
	}
	
# USEFUL to keep a HISTORY of RANDOM SEEDS ... 
# needle in haystack, but better than nothing 
memory.append = function(key, MEMORY="BASE", value) 
	{
	memory.log(key, MEMORY, "append");
	what = .GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]];
	n = length(what);
	if(n == 0)
		{
		what = list(value);
		} else {
				what[[n+1]] = value;
				}
	.GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]] = what;
	}



# memory.set TO DISK, not to .humanVerse as OPTION

	
	




minvisible.get = function(key="LAST")
	{
	memory.get(key, "-MINVISIBLE-");
	} 

minvisible = function(x, key="ANS", display=TRUE)
	{
##########################################################
##### I can't wrap this into a function check.string #####
##########################################################	
	ct.KEY = check.type(key);
	if(!ct.KEY || !is.character(key))	
		{ key = deparse(substitute(key)); } 
##########################################################

	memory.set(key, "-MINVISIBLE-", x);
	# also store to ANS variable ... 
	# I could do ANS = Ans = x;	 ANS %GLOBAL%. ; Ans %GLOBAL%. ;
	# ANS %GLOBAL% x;	# undefined ANS ... treated as "." (dot)
	
	key %GLOBAL% x; 
	
	
		
	# "Ans" %GLOBAL% x; 
	 
	
	ct.DISPLAY = check.type(display);
	# we don't care if it is a character 
	# 
	if(!ct.DISPLAY || !is.character(display))
		{ 
		display = deparse(substitute(display));		
		} else {
# we have a str = "lkdfsjdkljs"
# my keys for display are not longer than 10
				dlen = str.len(display);
				if(dlen > 10) { display = deparse(substitute(display));	}
				}
	display = prep.arg(display, n=3);
	
# dput(display);
	has.displayed = FALSE;
	if(!has.displayed && display == "str") 
					{ has.displayed = TRUE; print(str(x)); }
	if(!has.displayed && display == "head" || display == "hea") 
					{ has.displayed = TRUE; print(head(x)); }
	if(!has.displayed && display == "pip") 
					{ has.displayed = TRUE; print(pip(x)); } 
					# maybe write a print.pip method 
	if(!has.displayed && display == "print" || display == "pri") 
					{ has.displayed = TRUE; print(x); }
	if(!has.displayed &&	display == TRUE) 
					{ has.displayed = TRUE; print(x); }

	 
	invisible(x);	
	}




