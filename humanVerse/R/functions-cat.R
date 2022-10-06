
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
cat.warning = function(..., sep=" ")
	{
	str = prep.dots(...);
	res = paste0(str, collapse=sep);
	res = cat.color(res);
	
	parent.call = sys.call(sys.nframe() - 1L);
	res = paste("In", deparse(parent.call), ":", res);
	# res = str.wrap(); 
	warning( res , call.=FALSE);	
	}



cat.stop = function(..., sep=" ")
	{
	str = prep.dots(...);
	res = paste0(str, collapse=sep);
	res = cat.color(res);
	
	parent.call = sys.call(sys.nframe() - 1L);
	stop( paste("In", deparse(parent.call), ":", res) , call.=FALSE);	
	}


cat.norm = function(str, open=TRUE, set.as.wd = FALSE)
	{
	nstr = normalizePath(str);
	# nstr = cat.color(nstr);
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
 
 

log.cmd = function(what = "ping", id, res, open=FALSE)
	{
	f = "C:/_R_/-humanVerse-/SYSTEM/log/cmd/";
	p = paste0(f, what, "/");
	p = check.dir(p);
	
	if(open) { openSesame(p); }
	
	# /cmd/ping/id.log 
	o = paste0(p, id, ".log");
	o = check.file(o); # touch it ... 
	cat.log( o, what );
	cat.log( o, "");
	cat.log( o, id );
	cat.log( o, ""); 
	
	cat.dput(res, o);
	cat.log( o, ""); # end with \n 
	invisible(o);
	} 
   
cat.log = function(file, str, sep="\n", append=TRUE)
	{
	# ... logic is lost ... 
	cat(str, file=file, sep=sep, append=append);		
	}
	  
  
cat.dput = function(obj, file, one.line=FALSE, sep="\n", append=TRUE)
	{
	str = capture.output(dput(obj));
	if(one.line) { str = paste0(str, collapse=""); }
	cat(str, file=file, sep=sep, append=append);
	}
	
	# cat.dput(THIS, log);
	# dput( THIS, file=log, append=TRUE);
	# Error in dput(THIS, file = log, append = TRUE) : 
  # unused argument (append = TRUE)


.cat = function(..., sep="\n\n", psep = "", indent=0)
	{
	strs = prep.dots(...);
	strs = paste0(strs, sep=psep); # unaltered normally?
	if(indent > 0)
		{
		pads = str.rep(" ", indent);
		n = length(strs);
		res = character(n);
		for(i in 1:n)
			{
			lines = str.explode("\n", strs[i]);
			lines = paste0(pads, lines);
			res[i] = str.implode("\n", lines);
			}
		strs = res;
		}
	cat(sep);	cat(strs);	cat(sep);	
	}



cat.color = function(str, use.color.if.available=TRUE)
	{
	if(!use.color.if.available) { return ( strip.tags(str) ); }
	
	# is color available ??? ... has.ansi ... 
	
	
	strip.tags(str);
	}




 

check.colorCapability = function()
	{
	
# https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r

	# term <- Sys.getenv()["TERM"]
  # colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  # if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    # return(text)
  # }
	
	}
