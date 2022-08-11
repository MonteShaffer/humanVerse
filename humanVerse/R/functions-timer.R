
dots.append = function(val, ...)
	{
	more = unlist(list(...));
	val = c(val, more);
	val; 	
	}
	
	
	
timer.clearALL = function(notice=TRUE)
	{
	if(!exists("timer", .GlobalEnv$.humanVerse))
			{
			warning(paste0("Nothing to clear as timer.start has not been called yet!"));
			# next;
			} else {
					howMany = length(.GlobalEnv$.humanVerse[["timer"]]);
					.GlobalEnv$.humanVerse[["timer"]] = NULL;
					if(notice) { warning(paste0(" -- CLEAR from timers ALL [n  = ", howMany, "] was successful!")); }
					}		
	
	}

timer.clear = function(key="DEFAULT", ..., notice=TRUE)
	{ 
	keys = dots.append(key, ...);
	memory.init();	
	for(key in keys)
		{
		if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
			{
			warning(paste0("Nothing to clear as timer.start for key: ", key, " not called yet!"));
			next;
			} else {
					.GlobalEnv$.humanVerse[["timer"]][[key]] = NULL;
					if(notice) { warning(paste0(" -- CLEAR from timers [key] ", key, " was successful!")); }
					}		
		}
	
	}
	
	
##################################################
#'
#' timer.start
#'
#'
#' @param key (what is the unique storage 'key')
#'
#' @export
#'
#' @examples
timer.start = function(key="DEFAULT", ...)
	{ 
	keys = dots.append(key, ...);
	memory.init();	
	# SIMPLIFY with 'memory.set/get' ... add what to what 
	# .GlobalEnv$.humanVerse[["timer"]][[key]] = list();
	
	# this overwrites without any checks?
	now = Sys.time();
	for(key in keys)
		{
		.GlobalEnv$.humanVerse[["timer"]][[key]]$start = Sys.time();  # vs. proc.time()
		}
	}



	

##################################################
#'
#' timer.stop
#'
#'
#' @param key (what is the unique storage 'key')
#'
#' @export
#'
#' @examples	# marker is not multivariate ... 
timer.stop = function(key="DEFAULT", ..., marker="STOP-{n}")
	{
	keys = dots.append(key, ...);
	memory.init();
	
	
	now = Sys.time();
	for(key in keys)
		{
		if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
			{
			warning(paste0("Nothing to stop as timer.start for key: ", key, " not called yet!"));
			next;
			}
		
		diff = as.numeric(now) - as.numeric(.GlobalEnv$.humanVerse[["timer"]][[key]]$start);
		
		if(!exists("stop", .GlobalEnv$.humanVerse[["timer"]][[key]]))
			{
			# parallel vectors
			.GlobalEnv$.humanVerse[["timer"]][[key]]$stop = c(now);
			.GlobalEnv$.humanVerse[["timer"]][[key]]$diff = c(diff);
				mark = str.replace("{n}",1, marker);
			.GlobalEnv$.humanVerse[["timer"]][[key]]$marker = c(mark);
			} else {
					.GlobalEnv$.humanVerse[["timer"]][[key]]$stop = c(.GlobalEnv$.humanVerse[["timer"]][[key]]$stop, now);
					.GlobalEnv$.humanVerse[["timer"]][[key]]$diff = c(.GlobalEnv$.humanVerse[["timer"]][[key]]$diff, diff);
					howMany = (1 + length(.GlobalEnv$.humanVerse[["timer"]][[key]]$marker) ); 
					cat("\n", "howMany: ", howMany, "\n");
						mark = str.replace("{n}", howMany, marker);
					.GlobalEnv$.humanVerse[["timer"]][[key]]$marker = c(.GlobalEnv$.humanVerse[["timer"]][[key]]$marker, mark);
					}
		}	
	}













# can't use ... as already used ... dots[1], dots[2]
timer.printALL = function(format="seconds", 
							append.names=TRUE, 
							digits=2) 
	{
	forma = functions.cleanKey(format, 5, keep="-");
	if(!exists("timer", .GlobalEnv$.humanVerse))
			{
			warning(paste0("Nothing to print as timer.start has not been called yet!"));
			# next;
			} else {
					keys = names( .GlobalEnv$.humanVerse[["timer"]] );
					res = NULL;  # PANEL of DATA (PANEL FORM)
					for(key in keys)
						{
						x = timer.print(key, format=format, append.names=append.names, relative=TRUE, digits=digits);
						info = .GlobalEnv$.humanVerse[["timer"]][[key]];
						
						
						for(i in 1:length(x))
							{
							row = c(key, info$start, info$stop[i], info$diff[i], info$marker[i], forma, x[i]);
							res = rbind(res, row);
							}						
						}
					df = as.data.frame(res);
						rownames(df) = NULL; # seems to be weird (PRINT)
						colnames(df) = c("key", "start", "stop", "absolute", "marker", "format", "relative");
					df = df.setColumnType(df, c("start","stop","absolute"), "numeric"); 
					
					df$start = date.fromUnix(df$start); 
					df$stop  = date.fromUnix(df$stop);
					# last row is likely a string 
					df;
					}		
	
	
	
	}
	




##################################################
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
timer.print = function(key="DEFAULT", ..., 
							format="seconds", 
							append.names=TRUE, 
							relative=FALSE, 
							digits=2)
	{
	forma = functions.cleanKey(format, 5, keep="-");
	keys = dots.append(key, ...);
	memory.init();
	# seconds, pretty-seconds, pretty ...
	# port to obj.exists #NULL
	
	res = list();  # don't know the format yet
	for(key in keys)
		{
		if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
			{
			warning(paste0("Nothing to print as timer.start/timer.stop for key: ", key, " not called yet!"));
			next;
			}
		
		# absolute time from when you got on the bus and each STOP
		seconds = .GlobalEnv$.humanVerse[["timer"]][[key]]$diff;
		# relative time between STOPS
		if(relative) { seconds = c( seconds[1], diff(seconds) ); } # 
		
		seconds = property.set(seconds, "time.is", { if(relative) { "relative" } else { "absolute" } });

		# wrap into SWITCH?
		row = switch(forma,
						  "s"  	= seconds,   			# [s]econds 
						  "p" 	= timer.formatPretty(seconds, digits),	# [p]retty
						  "p-s"  	= timer.formatPrettySeconds(seconds, digits),
					seconds   # DEFAULT		
					);
			
		if(append.names) 
			{ 
			names(row) = .GlobalEnv$.humanVerse[["timer"]][[key]]$marker; 
			}
		res[[key]] = row;
		}	
	list.return(res);
	}
	

##################################################
#'
#' timer.formatPrettySeconds
#'
#'
#' @param seconds 
#'
#' @return
#' @export
#'
#' @examples	
timer.formatPrettySeconds = function(seconds, digits=2)
	{
	res = paste0( round(seconds, digits), " seconds");	
	names(res) = names(seconds);
	res;
	}

##################################################
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
timer.formatPretty = function(secondsV, digits=2)
	{
	# hack to multivariate
	n = length(secondsV);
	res = character(n);
	for(i in 1:n)
		{
		seconds = secondsV[i];
		str = "";
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
	names(res) = names(secondsV);
	res = property.set( res, property.getAll(secondsV) );
	res;
	}







