

timer.getOrigin = function()
	{
	# maybe these are also in MEMORY set ?
	structure(0, class = "POSIXct" ); # compact only
	} 
	
timer.tz = function()
	{
	# 
	tz = Sys.timezone();
	# good data analytics would include this ELEMENT ... how to append to everything ... 
	# doesn't look like tz="GMT" or tz="UTC" works ...in as.POSIXct 
	tz;
	}
	
	


# https://www.alexejgossmann.com/benchmarking_r/
# microbenchmark ... C getting to nanoticker ... rbenchmark, one 'R' function by DIRK ... LOL tinyverse 
# if you want to clear internals, use timer.clear ... or timer.clearALL

timer.init = function(purge.memory = FALSE, 
							show.msg = TRUE)
	{
	memory.init();
#cat("\n INIT 0 ... \n\n");	
	#################################################
	if( purge.memory || is.null(memory.get("timer", "TIMERS")) )
		{
#cat("\n INIT 1 ... \n\n");
		# public timers 
		memory.start("timer", "TIMERS", 
								params = list("tz" = timer.tz())
					);		
		}
	if( purge.memory || is.null(memory.get(".timer", "TIMERS")) )
		{
#cat("\n INIT 2  ... \n\n");
		# private (system) timers  [AUTOSAVE]
		memory.start(".timer", "TIMERS", 
								params = list("tz" = timer.tz())
					);		
		}
		

	PUBLIC_TIMER = "timer"; 
	PUBLIC_OBJ = memory.get(PUBLIC_TIMER, "TIMER");
	# THIS should only be called once, 
	#### but every time, gets a new INIT timer 
		now = as.numeric(Sys.time());
		
	N_PUBLIC = length(PUBLIC_OBJ);
	
	PRIVATE_TIMER = ".timer"; 
	PRIVATE_OBJ = memory.get(PRIVATE_TIMER, "TIMER");
	N_PRIVATE = length(PRIVATE_OBJ);
		## INTERNAL RAINBOW TABLE ... 
		## if we save .humanVerse (autosave), we can recover MD5 from previous sessions
		## PRIVATE NAME 
		md5 = str.toMD5( (now) );
		key = paste0("SESSION-", md5 );	
#dput(key);
	memory.set(md5, "MD5", paste0("PRIVATE_TIMER:SESSION-",now) ); 
		## PUBLIC NAME is key 
		# so don't have to parse, and have a lookup
	timer.start( key , as.internal = TRUE);  # recursion on timer.start ...
	if(show.msg) 
		{  
		# could "cat" have a color?
		# https://www.r-project.org/nosvn/pandoc/crayon.html
		# It is easy to define your own themes:
		# only works in RStudio, not RGui (windows)
		# works in RGui on Debian ... 
		# Does the current R session support ANSI colors?
		# num_ansi_colors() > 1L
		# has_hyperlink
		## Are we in a terminal? No?
		# if (!isatty(stdout())) { return(FALSE) }
		# ansi_colors_256()
		# show_ansi_colors()
		# styles()
		
		msg = str.commentWrapper("Welcome to the {humanVerse}");
		# maybe store this colorized form as a global HUMANVERSE
		# have append final length, and left, pad ...
		

		cat( msg ); 
		cat("\n", "The [timer] module is running", "\n");
		cat("\n", "\t\t", "Currently, there are:",  "\n");
		cat("\t\t\t\t", "[", N_PUBLIC , "] USER ", str.grammaticalNumber("timer",n), "running", "\n");
		cat("\t\t\t\t", "[", N_PRIVATE, "] INTERNAL ", str.grammaticalNumber("timer",n), "running", "\n");
		cat("\n\n", "Just now, an INTERNAL timer started: [", key, "]", "\n\n\n");
		
		}
	}
	
	
	
	 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' timer.start
#'
#'
#' @param key (what is the unique storage 'key')
#'
#' @export
#'
#' @examples
timer.start = function(key="DEFAULT", ..., 
							as.internal = FALSE)
	{
	keys = dots.addToKey(key, ...);
	
	TIMER = "timer"; if(as.internal) { TIMER = ".timer"; }
	
	memory.init();
	
###############################################
	timer = memory.get(TIMER, "TIMERS");
	if(is.null(timer)) { timer.init(); } # maybe "quietly", recursion if BUG
###############################################
	
	now = Sys.time(); 		# vs. proc.time()
	for(key in keys)
		{
		timer[[key]]$start = now;  
		}
cat("\n timer with key :: [",key,"] has started \n\n");
###############################################
	memory.set(TIMER, "TIMERS", timer);
###############################################
	}



	

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' timer.stop
#'
#'
#' @param key (what is the unique storage 'key')
#'
#' @export
#' 
#' @examples	# marker is not multivariate ... 
## TODO:: can I shorten with `with`
timer.stop = function(key="DEFAULT", ..., marker="STOP-{n}", 
							as.internal = FALSE)
	{
	keys = dots.addToKey(key, ...);
	
	TIMER = "timer"; if(as.internal) { TIMER = ".timer"; }
	
	memory.init();
	
###############################################
	timer = memory.get(TIMER, "TIMERS");
	if(is.null(timer)) { timer.init(); } # maybe "quietly", recursion if BUG
###############################################
	
	now = Sys.time();
	n.keys = length(keys);
	res = list();
	relative = numeric(n.keys);
	i = 0;
	for(key in keys)
		{
		i = 1 + i;
		if(is.null(timer[[key]]))
			{
			warning.cat("Nothing to stop as timer.start for key: ", key, " not called yet!");
			next;
			}
		
		diff = as.numeric(now) - as.numeric(timer[[key]]$start);
		
		if(is.null(timer[[key]]$stop))
			{
			# store parallel-length vectors
			timer[[key]]$stop = c(now);
			# absolute difference 
			timer[[key]]$diff = c(diff);
				mark = str.replace("{n}",1, marker);
			timer[[key]]$marker = c(mark);
			} else {
					timer[[key]]$stop = c(timer[[key]]$stop, now);
					timer[[key]]$diff = c(timer[[key]]$diff, diff);
					howMany = (1 + length(timer[[key]]$marker) ); 
					cat("\n", "howMany: ", howMany, "\n");
						mark = str.replace("{n}", howMany, marker);
					timer[[key]]$marker = c(timer[[key]]$marker, mark);
					}
		nstops = length(timer[[key]]$stop);
		relative[i] = diff; 
		if(nstops > 1) 
			{ 
			relative[i] = as.numeric(timer[[key]]$stop[nstops]) - as.numeric(timer[[key]]$stop[nstops - 1]) ;
			}
		
		res[[i]] = timer.formatPrettyUnits(relative[i], "seconds");
# cat("\n", "KEY: [",key,"]", "\t\t", "RELATIVE TIME AT [",mark,"] \t ", res[[i]], "\n");
		
cat("\n timer with key :: [",key,"] has recorded *a* stop labeled [",mark,"] \n\t\t\t at absolute time: [",diff,"] \t which is \n\t\t\t at relative time:: [",relative[i],"] \t *to the previous stop* \n\n");  
		}
###############################################
	memory.set(TIMER, "TIMERS", timer);
###############################################
	minvisible(relative, print=FALSE); 
	}
















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' timer.print
#'
#'
#' @param key (what is the unique storage 'key')
#' @param format (how to return the timer time)
#'
#' @return
#' @export
#'
#' @examples
# format = [s]econds, [p]retty, [p]retty-[s]econds 
timer.print = function(key="DEFAULT", ..., 
							format="seconds", 
							units.name = "seconds",
							units.factor = 1,
							append.names=TRUE, 
							time.is = "relative", 
							digits=2, 
							as.internal = FALSE)
	{
	keys 	= dots.addToKey(key, ...);
	
	TIMER 	= "timer"; if(as.internal) { TIMER = ".timer"; }
	
	forma 	= prep.arg(format, 1, keep="-");
	tim 	= prep.arg(time.is, 3);
	
	memory.init();
	
###############################################
	timer = memory.get(TIMER, "TIMERS");
	if(is.null(timer)) { timer.init(); } # maybe "quietly", recursion if BUG
###############################################
	# seconds, pretty-seconds, pretty ...
	# port to obj.exists #NULL
	
	res = list();  # don't know the format yet
	for(key in keys)
		{
		if(is.null(timer[[key]]))
			{
			warning(paste0("Nothing to print as timer.start/timer.stop for key: ", key, " not called yet!"));
			next;
			}
		
		# absolute time from when you got on the bus and each STOP
		seconds = timer[[key]]$diff;
		# relative time between STOPS
		if(tim == "rel") { seconds = c( seconds[1], diff(seconds) ); } # 
		

		vkey = units.name;
		vfactor = as.numeric(units.factor);
		vals = as.numeric(seconds) * vfactor;
		vals = property.set("time.is", vals, { if(tim == "rel") { "relative" } else { "absolute" } });

cat("\n", " FORMA: ", forma, "\n\n");		
		# wrap into SWITCH?
		row = switch(forma,
						  "s"  	= vals,   			# [s]econds 
						  "p" 	= timer.formatPretty(vals, vkey, vfactor, digits),	# [p]retty
						  "p-s"  	= timer.formatPrettyUnits(vals, vkey, digits), # [p]retty-[s]econds
					vals   # DEFAULT		
					);
			
		if(append.names) 
			{ 
			names(row) = timer[[key]]$marker; 
			}
		res[[key]] = row;
		}	
	res = list.return(res);  
	res = property.set("time.is", res, time.is);
	res;
	}
	









 
 
timer.printALL = function(key="---PRINT-EVERYTHING---", ..., format="pretty", 
							units.name = "seconds",
							units.factor = 1,
							append.names=TRUE, 
							digits=2, 
							as.internal = FALSE)
	{
	if(key != "---PRINT-EVERYTHING---")
		{
		keys = dots.addToKey(key, ...);
		} else { keys = NULL; }
	TIMER = "timer"; if(as.internal) { TIMER = ".timer"; }
	
	forma = prep.arg(format, 5, keep="-");

###############################################
	timer = memory.get(TIMER, "TIMERS");
	if(is.null(timer)) { timer.init(); } # maybe "quietly", recursion if BUG
###############################################	
	if(is.null(timer))
			{
			warning(paste0("Nothing to print as timer.start has not been called yet!"));
			# next;
			} else {
					allkeys = names( timer );
					if(is.null(keys)) { keys = allkeys; }
					j = 1;
					res = NULL;  # PANEL of DATA (PANEL FORM)
					for(key in keys)
						{
						# I could pass "forma" here, but why?
						x = timer.print(key, format=format, 
											units.name=units.name,
											units.factor=units.factor,
											append.names=append.names, 
											time.is = "relative", 
											digits=digits
										);
										
						info = timer[[key]];
						
						for(i in 1:length(x))
							{
							row = c(j, key, info$start, info$stop[i], info$diff[i], info$marker[i], format, x[i]);
							res = rbind(res, row);
							j = 1 + j;
							}	
						
						}

					df = as.data.frame(res);
					
						rownames(df) = NULL; # seems to be weird (PRINT)
						colnames(df) = c("idx", "key", "start", "stop", "absolute", "marker", "format", "relative");
		
					# df = df.setColumnType(df, c("start","stop","absolute"), "numeric"); 
					df$idx = as.integer(df$idx);
					df$start = as.POSIXct( as.numeric(df$start), origin=timer.getOrigin() );
					df$stop = as.POSIXct( as.numeric(df$stop), origin=timer.getOrigin() );
					df$absolute = ( as.numeric(df$absolute) );
					# last column is likely a string 
					return(df);
					}	
	}
	





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' timer.formatPrettyUnits
#'
#'
#' @param seconds 
#'
#' @return
#' @export
#'
#' @examples	
timer.formatPrettyUnits = function(vals, vkey="seconds", digits=2)
	{
	# vals have already been vfactor-ed 
	res = paste0( round(vals, digits), " ", vkey);	
	names(res) = names(vals);
	res;
	}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' timer.formatPretty
#'
#'
#' @param seconds 
#'
#' @return
#' @export
#'
#' @examples	
# https://stackoverflow.com/questions/572049/convert-seconds-to-days-minutes-and-hours-in-obj-c
timer.formatPretty = function(vals, vkey="seconds", vfactor=1, digits=2)
	{
	# this currently only works "seconds" ... 
	# FOR others, we would just have to adjust vals to be in seconds ... 
	# hack to multivariate
	n = length(vals);
	res = character(n);
	do.micro = FALSE;
	if(vfactor > 1) { do.micro = TRUE; }
	for(i in 1:n)
		{
		str = "";
		
		
		seconds = as.numeric( vals[i] / vfactor);  # milliseconds, rescaled ...
		
		days = floor( seconds / (60 * 60 * 24) );
			seconds = seconds - days * (60 * 60 * 24);
					dstr = "days"; if(days == 1) { dstr = "day"; }
					if(days > 0) { str = paste0(str, days," ",dstr,", "); }
		hours = floor( seconds / (60 * 60) );
					hstr = "hours"; if(hours == 1) { hstr = "hour"; }
					if(days > 0 | hours > 0) { str = paste0(str, hours," ",hstr,", "); }
			seconds = seconds - hours * (60 * 60);
		minutes = floor( seconds / 60 );
					mstr = "minutes"; if(minutes == 1) { mstr = "minute"; }
					if(days > 0 | hours > 0 | minutes > 0) { str = paste0(str, minutes," ",mstr,", "); }
			seconds = seconds - minutes * (60);

		res[i] = paste0( str, round(seconds, digits), " seconds");		
		}
	names(res) = names(vals);
	# trying to append and keep "time.is" 
	# res = property.set( property.getAll(vals), res, );
	res;
	}





timer.clear = function(key="DEFAULT", ..., 
							notice=TRUE, 
							as.internal = FALSE)
	{
	keys = dots.addToKey(key, ...);
	
	TIMER = "timer"; if(as.internal) { TIMER = ".timer"; }
	
	memory.init();
	
###############################################
	timer = memory.get(TIMER, "TIMERS");
	if(is.null(timer)) { timer.init(); } # maybe "quietly", recursion if BUG
###############################################
	
	for(key in keys)
		{
		if(is.null(timer[[key]]))
			{
			warning(paste0("Nothing to clear as timer.start for key: ", key, " not called yet!"));
			next;
			} else {
					timer[[key]] = NULL;
					if(notice) { warning(paste0(" -- CLEAR from timers [key] ", key, " was successful!")); }
					}		
		}
###############################################
	memory.set(TIMER, "TIMERS", timer);
###############################################
	}



timer.clearALL = function(notice=TRUE, call.init=TRUE, 
							as.internal = FALSE)
	{
	TIMER = "timer"; if(as.internal) { TIMER = ".timer"; }

	memory.init();
###############################################
	timer = memory.get(TIMER, "TIMERS");
	if(is.null(timer)) { timer.init(); } # maybe "quietly", recursion if BUG
###############################################	
	# this will erase ALL timers, 
	# including INTERNALS (e.g., AUTOSAVE, if as.internal=TRUE)
	# AUTOSAVE / RESTORE have timestamps as keys,
	#     so, you could RESTORE and show HOW LONG between STEPS
	purge = FALSE;
	if(is.null(timer))
			{
			warning(paste0("Nothing to clear as timer.start has not been called yet!"));
			# next;
			} else {
					howMany = length(timer);
					timer = NULL;
					purge = TRUE;
					if(notice) { warning(paste0(" -- CLEAR from timers ALL [n  = ", howMany, "] was successful!")); }
					}		
###############################################
	memory.set(TIMER, "TIMERS", timer);
###############################################
	if(call.init && purge) { timer.init(); }
	}



 