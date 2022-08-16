
init = function() {}  
# this function is linked in .onLoad()
# if we know WHERE a local config file is, go grab it ... 
# we should check colors and store it... ENABLE 



##################################################
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


memory.clear = function(purge.ls=TRUE, all.names=TRUE, purge.gc=TRUE, detach.packages=TRUE)
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






memory.defaults = function() {}

memory.getDefaults
memory.setDefaults



memory.get = function() {}


# obj = .GlobalEnv$.humanVerse
memory.set = function(key, ... , value, parent=NULL) 
	{
	if(is.null(parent)) { parent = .GlobalEnv$.humanVerse; }
	
	.GlobalEnv$.humanVerse[["timer"]][[key]] = list();
	
		within(.GlobalEnv$.humanVerse)
		{
		
		}
		
		
	memory.check("timer");
	
	}
	
	
	

