
warning.clear = function()
	{
	# https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings
	assign("last.warning", NULL, envir = baseenv());
	# unlockBinding("last.warning", baseenv());
	# If warn is zero (the default), a read-only variable last.warning is created.
	}

warnings.clear = warning.clear;
clear.warnings = warning.clear;
clear.warning = warning.clear;

# https://stackoverflow.com/questions/9596918/r-warning-wrapper-raise-to-parent-function
warning.cat = function(..., sep=" ")
	{
	str = dots.addTo(NULL, ...);
	res = paste0(str, collapse=sep);
	
	parent.call = sys.call(sys.nframe() - 1L);
	warning( paste("In", deparse(parent.call), ":", res) , call.=FALSE);	
	}

cat.warning = warning.cat;

stop.cat = function(..., sep=" ")
	{
	str = dots.addTo(NULL, ...);
	res = paste0(str, collapse=sep);
	
	parent.call = sys.call(sys.nframe() - 1L);
	stop( paste("In", deparse(parent.call), ":", res) , call.=FALSE);	
	}

cat.stop = stop.cat;


cat.init = function()
	{
	# load objects, where to attach?
	
	# [ESC]
	# colors8, colors16, colors256
	# <i><b><fg color="#ff0000"><bg color="black"></bg></fg></b></i><reset />
	# use HTML-like tags ... 
	# <esc custom="a" more="b"></esc>  # a generic ESC event 
	# COLOR is attached to either <fg> or <bg>
	
	
	
	}


