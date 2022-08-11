
# s = "(a(a(a)(aa(a)a)a)a)((b(b)b)b)(((cc)c)c)"
# matched <- gregexpr("\\((?>[^()]|(?R))*\\)", s, perl = T)
# https://regex101.com/r/iqJ5Pi/1
# THIS GETS nest, use STR, REPLACE TO FIND REMAINING ? OR/AND 
# once in a NEST, remove outer parentheses, call REGEX again 
# substring(s, matched[[1]], matched[[1]] + attr(matched[[1]], "match.length") - 1)

pattern = "\\((?>[^()]|(?R))*\\)";
subject = c( "(a(a(a)(aa(a)a)a)a)((b(b)b)b)(((cc)c)c)", "(a(a(a)(aa(a)a)a)a) OR ((b(b)b)b) AND (((cc)c)c)" );

subject = "(a(a(a)(aa(a)a)a)a) OR ((b(b)b)b) AND (((cc)c)c)";

match = gregexpr( pattern, subject, perl = T);
	start = as.numeric(match[[1]]); length = attr(match[[1]], "match.length");
	substring(subject, start, (start + length) - 1);
	
	# we can just count from 1:strlen and rebuild "MISSING" elements
	
subject = "a(a(a)(aa(a)a)a)a"; # remove outer parentheses, do again 


 

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


 



