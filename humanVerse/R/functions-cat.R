
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
	str = prep.dots(...);
	res = paste0(str, collapse=sep);
	
	parent.call = sys.call(sys.nframe() - 1L);
	res = paste("In", deparse(parent.call), ":", res);
	# res = str.wrap(); 
	warning( res , call.=FALSE);	
	}

cat.warning = warning.cat;

stop.cat = function(..., sep=" ")
	{
	str = prep.dots(...);
	res = paste0(str, collapse=sep);
	
	parent.call = sys.call(sys.nframe() - 1L);
	stop( paste("In", deparse(parent.call), ":", res) , call.=FALSE);	
	}

cat.stop = stop.cat;






cat.norm = function(str, open=TRUE, set.as.wd = FALSE)
	{
	nstr = normalizePath(str);
	cat( nstr );
	if(open) { utils::browseURL(nstr); }
	if(set.as.wd) { setwd( nstr ); }
	}



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




msg.badOption = function(KEY="method", 
								OPTION="Original-Entry", 
								SHORTCODE="ori-ent")
		{
		prep.msg("\n\n\t",
					"It appears that you entered an INCORRECT",
					"<v>[</v>", KEY, "<v>]</v>", 
					"\n\n\t",
					"You entered: ", "<v>[</v>", OPTION, "<v>]</v>",
					"\n\n\t\t",
					"which was 'short-coded' to: ", "<v>[</v>", SHORTCODE, "<v>].</v>",
					"\n\n",
					"Please try again.",
					"\n\n",					
					"Above is a list of options with allowed 'shortcodes'.",
					"\n\n",
					"Which can be retrieved as a dataframe using ",
					"<v>* ANS *</v> ", 
					"\n\n"
					);		
		}
	
	

msg.missingParam = function(PARAM="f", DEFAULT=1)
		{
		prep.msg("\n\n\t",
					"It appears that you are missing a PARAMETER",
					"<v>[</v>", PARAM, "<v>]</v>", 
					"\n\n\t",
					"Assigning to DEFAULT value ", "<v>[</v>", DEFAULT, "<v>]</v>",
					"\n\n\t\t",
					"The result may not be what you intended.  You may want to try again.",
					"\n\n"
					);		
		}
