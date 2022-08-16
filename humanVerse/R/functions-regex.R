












# str contains the wildcard operator `*` [map any chars] or `?` [map single char]
# it wraps around START / END, options to edit
# it will build a 
# is INTERNAL better than PERL... default ... perl = FALSE
regex.prepWildcard = function(search.term, ...)
	{
	utils::glob2rx(search.term, ...);
	}
	
	 
# grep(value = FALSE) ... return's idx ... 
regex.wildcardSearch = function(searching.what, search.term, ignore.case=TRUE, value = FALSE, ...)
	{
	grx = regex.prepWildcard(search.term, ...);
	idx = grep(grx, searching.what , ignore.case = ignore.case, value = value, ... );
	## idx of elements 
	idx;
	}

# grx = utils::glob2rx("mon*");
# grep(grx, c("monte", "MONTANA"), ignore.case=TRUE, perl=TRUE);
# grep() ... shows the indexes of matches ... TRUE / FALSE 














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


 



