

timer.start = function(key="DEFAULT")
	{
	# this overwrites without any checks?
	.GlobalEnv$.humanVerse[["timer"]][[key]] = list();
	.GlobalEnv$.humanVerse[["timer"]][[key]]$start = Sys.time();  # vs. proc.time()
	}
	
timer.stop = function(key="DEFAULT")
	{
	if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
		{
		stop(paste0("Nothing to stop as timer.start for key: ", key, " not called yet!"));
		}
	.GlobalEnv$.humanVerse[["timer"]][[key]]$stop = Sys.time();
	
	.GlobalEnv$.humanVerse[["timer"]][[key]]$diff = as.numeric(.GlobalEnv$.humanVerse[["timer"]][[key]]$stop)-as.numeric(.GlobalEnv$.humanVerse[["timer"]][[key]]$start);
	}


timer.print = function(key="DEFAULT", return="seconds")
	{
	# seconds, pretty-seconds, pretty ...
	if(!exists(key, .GlobalEnv$.humanVerse[["timer"]]))
		{
		stop(paste0("Nothing to print as timer.start/timer.stop for key: ", key, " not called yet!"));
		}
	
	seconds = .GlobalEnv$.humanVerse[["timer"]][[key]]$diff;
		
	if(return == "pretty-seconds") { return( timer.pretty.seconds(seconds) ); }
	
	if(return == "pretty") { return( timer.pretty(seconds) ); }
	
	# DEFAULT
	seconds;
	}
	
	
timer.pretty.seconds = function(seconds)
	{
	paste0( round(seconds,2), " seconds");	
	}

# https://stackoverflow.com/questions/572049/convert-seconds-to-days-minutes-and-hours-in-obj-c
timer.pretty = function(seconds)
	{
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

	paste0( str, round(seconds,2), " seconds");		
	}






