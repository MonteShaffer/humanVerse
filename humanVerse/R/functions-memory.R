




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






 

memory.get = function(key, MEMORY="BASE", unused=NULL) 
	{	
	.GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]];
	}


# 
	# if(is.null(.GlobalEnv$.humanVerse)) { memory.init(); }
	# if(is.null(.GlobalEnv$.humanVerse["."])) { .GlobalEnv$.humanVerse["."] = list(); }
	# if(is.null(.GlobalEnv$.humanVerse["."][[MEMORY]])) { .GlobalEnv$.humanVerse["."][[MEMORY]] = list(); }
# different than property.set (key, OBJ, Value)
# now the same ... KEY on OBJ to VAL
memory.set = function(key, MEMORY="BASE", value) 
	{
	.GlobalEnv$.humanVerse[["."]][[MEMORY]][[key]] = value;	
	}
	

# memory.set TO DISK, not to .humanVerse as OPTION

	
	

