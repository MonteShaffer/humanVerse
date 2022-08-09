


 

# see preg_match at PHP
regex.match = function(pattern="(^[^:]+):(.+)", 
						subject=c("Archs: x64","Encoding: UTF-8"),
						perl=TRUE)
	{

	r = regexec(pattern, subject, perl=perl); ### THIS WORKS
	s = regmatches(subject, r);

	slen = length(s);

	if(slen == 0) { return(FALSE); }
	if(slen == 1 && length(s[[1]]) == 0 ) { return(FALSE); }

	list.return(s);
	}


 



