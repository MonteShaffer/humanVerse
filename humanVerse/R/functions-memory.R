




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' memory.init;
#'
#' @param purge.memory
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples 
memory.init = function(purge.memory = FALSE, verbose = TRUE)
  {
  if(!exists(".humanVerse") || purge.memory)
    {
    if(verbose)
      {
      cat("humanVerse::memory.init; ... initializing HIDDEN list '.humanVerse'", "\n");
      }

    .GlobalEnv$.humanVerse = list();
	}
	# this gets called often 
	# system...auto save ... with timer HIDDEN/INTERNAL 
	# ["timers"] as variable ... ["timers-INTERNAL"] 
  }


#' @rdname initMemory
#' @export
initMemory = memory.init;








# getBASE64 key from Sys.getenv("humanVerse")
# path to file of config ... if exists, load config
# # memory.smartRestore();


system.clear = function(purge.ls=TRUE, all.names=TRUE, purge.gc=TRUE, detach.packages=TRUE)
	{
	# does this unregister loaded libraries?
	if(purge.ls) { rm(list=ls(all.names=all.names)); }
	if(purge.gc) { gc(); }
	if(detach.packages) { }
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

 

memory.log = function(key, MEMORY, action="get")
	{
	# manual set the value so no recursion 
	# we are logging timestamps, not values ... 
		now = as.POSIXct(Sys.time());
		info = df.row(now, action, MEMORY, key, use.names=TRUE); # works nicely 
	what = .GlobalEnv$.humanVerse[["."]][["-SYSTEM_LOG-"]];
	if(is.null(what)) { what = info; } else { what = rbind(what,info); }
	.GlobalEnv$.humanVerse[["."]][["-SYSTEM_LOG-"]] = what;
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

minvisible = function(x, key="LAST", display=TRUE)
	{
	memory.set(key, "-MINVISIBLE-", x);
	# also store to ANS variable ... 
	# I could do ANS = Ans = x;   ANS %GLOBAL%. ; Ans %GLOBAL%. ;
	# ANS %GLOBAL% x;  # undefined ANS ... treated as "." (dot)
	"ANS" %GLOBAL% x; 
	"Ans" %GLOBAL% x; 
	
	
	ct.DISPLAY = check.type(display);
	if(!ct.DISPLAY || !is.character(display))	
		{ 
		display = deparse(substitute(display)); 
		display = prep.arg(display, n=3);
		}
	# str may be object ... str may be a var in GLOBAL ... str="hello world";
# dput(display);
	has.displayed = NULL;
	if(is.null(has.displayed) && display == "str") 
		{ has.displayed = TRUE; print(str(x)); }
	if(is.null(has.displayed) && display == "hea") 
		{ has.displayed = TRUE; print(head(x)); }
	if(is.null(has.displayed) && display == "pip") 
		{ has.displayed = TRUE; print(pip(x)); } 
		# maybe write a print.pip method 
	if(is.null(has.displayed) && display == "print") 
		{ has.displayed = TRUE; print(x); }
	if(is.null(has.displayed) && display == TRUE) 
		{ has.displayed = TRUE; print(x); }

	 
	invisible(x);	
	}




