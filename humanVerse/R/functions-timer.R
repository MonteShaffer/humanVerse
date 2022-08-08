
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
timer.start = function(key="DEFAULT")
	{
	memory.init();
	# this overwrites without any checks?
	.GlobalEnv$.humanVerse[["timer"]][[key]] = list();
	.GlobalEnv$.humanVerse[["timer"]][[key]]$start = Sys.time();  # vs. proc.time()
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
#' @examples	
timer.stop = function(key="DEFAULT", marker="STOP-{n}")
	{
	memory.init();
	if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
		{
		stop(paste0("Nothing to stop as timer.start for key: ", key, " not called yet!"));
		}
	
	now = Sys.time();
	diff = as.numeric(now) - as.numeric(.GlobalEnv$.humanVerse[["timer"]][[key]]$start);

	if(!exists("stop", .GlobalEnv$.humanVerse[["timer"]][[key]]))
		{
		# parallel vectors
		.GlobalEnv$.humanVerse[["timer"]][[key]]$stop = c(now);
		.GlobalEnv$.humanVerse[["timer"]][[key]]$diff = c(diff);
			marker = str.replace("{n}",1, marker);
		.GlobalEnv$.humanVerse[["timer"]][[key]]$marker = c(marker);
		} else {
				.GlobalEnv$.humanVerse[["timer"]][[key]]$stop = c(.GlobalEnv$.humanVerse[["timer"]][[key]]$stop, now);
				.GlobalEnv$.humanVerse[["timer"]][[key]]$diff = c(.GlobalEnv$.humanVerse[["timer"]][[key]]$diff, diff);
					marker = str.replace("{n}", (1 + length(.GlobalEnv$.humanVerse[["timer"]][[key]]$marker) ), marker);
				.GlobalEnv$.humanVerse[["timer"]][[key]]$marker = c(.GlobalEnv$.humanVerse[["timer"]][[key]]$marker, marker);
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
timer.print = function(key="DEFAULT", format="seconds", names=TRUE, relative=TRUE, digits=2)
	{
	memory.init();
	# seconds, pretty-seconds, pretty ...
	if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
		{
		stop(paste0("Nothing to print as timer.start/timer.stop for key: ", key, " not called yet!"));
		}
	
	# absolute time from when you got on the bus and each STOP
	seconds = .GlobalEnv$.humanVerse[["timer"]][[key]]$diff;
	# relative time between STOPS
	if(relative) { seconds = c( seconds[1], diff(seconds) ); } # 
	
	seconds = property.set("time.is", { if(relative) { "relative" } else { "absolute" } }, seconds);

	if(names) { names(seconds) = .GlobalEnv$.humanVerse[["timer"]][[key]]$marker;}

	# wrap into SWITCH?
		
	if(format == "pretty-seconds") { return( timer.formatPrettySeconds(seconds, digits) ); }
	
	if(format == "pretty") { return( timer.formatPretty(seconds, digits) ); }
	
	# DEFAULT
	seconds;
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







