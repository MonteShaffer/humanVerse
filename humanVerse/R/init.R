
# onload
init = function() 
	{
	memory.init();
	
	# load .humanVerse from CONFIG and use its values, otherwise 
	
	
	par.saveStateInitial();
	options.saveStateInitial();
	system.saveStateInitial(); 
	timer.init(show.message=FALSE);  # this captures the local tz 
	
	# maybe add wrapper for formals athat allows base::as.POSIXct for clarity
	# doesn't seem to work ... 
	formals(as.POSIXct) = alist(x = , tz="UTC", ... = );
	formals(as.POSIXlt) = alist(x = , tz="UTC", ... = );
	# formalArgs(as.Date)
	# getFunction(name, generic=TRUE, mustFind=TRUE, where)
	# functionBody
	# 


	}  
# this function is linked in .onLoad()
# if we know WHERE a local config file is, go grab it ... 
# we should check colors and store it... ENABLE 

# onUnload
unload = function() { }  

  # memory.init();
  # memory.smartRestore(); # ... onUnload saveState ...