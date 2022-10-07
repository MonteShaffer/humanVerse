
# https://stackoverflow.com/questions/986937/how-can-i-get-the-browsers-scrollbar-sizes
# https://www.thewindowsclub.com/how-to-change-scrollbar-width-in-chrome-and-firefox#:~:text=On%20the%20Options%20page%2C%20you,change%20the%20width%20of%20scrollbars.
# NOT easy, why is that ... 	
	
	
# onload
init = function() 
	{
	SESSION_HV = .uniqid();  # "1665150822.487980.8254b"
	SESSION_HV %GLOBAL%. ;
	
	SESSION_R = .session();	 # "WKDevo" ==> "58a0debe8" ... debbie dallas?
	SESSION_R %GLOBAL%. ;
	
	memory.init();
	
	# load .humanVerse from CONFIG and use its values, otherwise 
	########## ._____init.settings();
	
	par.saveStateInitial();
	options.saveStateInitial();
	system.saveStateInitial(); 
	timer.init(show.message=FALSE);  # this captures the local tz 
	
	# maybe add wrapper for formals athat allows base::as.POSIXct for clarity
	# doesn't seem to work ... 
	##### formals(as.POSIXct) = alist(x = , tz="UTC", ... = );
	##### formals(as.POSIXlt) = alist(x = , tz="UTC", ... = );
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