
ansi_regex <- paste0("(?:(?:\\x{001b}\\[)|\\x{009b})",
                     "(?:(?:[0-9]{1,3})?(?:(?:;[0-9]{0,3})*)?[A-M|f-m])",
                     "|\\x{001b}[A-M]");
					 
					 
has_style <- function(string) {
  grepl(ansi_regex, string, perl = TRUE)
}

strip_style <- function(string) {
  gsub(ansi_regex, "", string, perl = TRUE, useBytes = TRUE)
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



cat.checkColorCapability