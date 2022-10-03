

str.onlyAlphaNumeric = function(str)
	{
	gsub(REGEX_NOT_ALPHA_NUMERIC, "", str);	
	}


# https://stackoverflow.com/a/43775302/184614
# You may remove any 1+ non-ASCII symbols with a [^ -~]+ regex:
#> gsub("[^ -~]+", "", "I mean totally \xed\xa0\xbd\xed\xb8\x8a")

# https://stackoverflow.com/a/32175520/184614
# I would strongly recommend only using \u, as it's much less error-prone.
# \x consumes 1-4 characters, so long as they're hex digits - whereas \u must always be followed by 4 hex digits.  

regex.replace = function(string, regex, FN.elem)
	{
	# for each element of the "match", call the function 'FN.elem'
	
	
	
	}

regex.test = function(search.term, regex)
	{
	# multivariate true or false?
	
	}

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













# s = "(a(a(a)(aa(a)a)a)a)((b(b)b)b)(((cc)c)c)"
# matched <- gregexpr("\\((?>[^()]|(?R))*\\)", s, perl = T)
# https://regex101.com/r/iqJ5Pi/1
# THIS GETS nest, use STR, REPLACE TO FIND REMAINING ? OR/AND 
# once in a NEST, remove outer parentheses, call REGEX again 
# substring(s, matched[[1]], matched[[1]] + attr(matched[[1]], "match.length") - 1)

### pattern = "\\((?>[^()]|(?R))*\\)";
##subject = c( "(a(a(a)(aa(a)a)a)a)((b(b)b)b)(((cc)c)c)", "(a(a(a)(aa(a)a)a)a) OR ((b(b)b)b) AND (((cc)c)c)" );

##subject = "(a(a(a)(aa(a)a)a)a) OR ((b(b)b)b) AND (((cc)c)c)";

# match = gregexpr( pattern, subject, perl = T);
	# start = as.numeric(match[[1]]); length = attr(match[[1]], "match.length");
	# substring(subject, start, (start + length) - 1);
	
	# # we can just count from 1:strlen and rebuild "MISSING" elements
	
# subject = "a(a(a)(aa(a)a)a)a"; # remove outer parentheses, do again 


 

 













#' wildcardSearch
#'
#' By default searches a dataframe string column based on a
#'  wildcard `*` operator.
#'
#' Can also just search a single character vector.
#'
#' @param str Search string with basic wildcard `*` operator
#' @param col.name The column to perform search on [or a character vector]
#' @param return.cols list of columns you want to display, by default all
#'
#' @param ignore.case Defaults to TRUE, matches "mont" and "Mont"
#' @param perl Defaults to FALSE, convert string query to PERL regex?
#' @param df Dataframe to search [column]

#'
#' @return dataframe of results, empty dataframe is possible
#' @export
wildcardSearch = function(str, col.name, return.cols=NULL, ignore.case=TRUE, perl=FALSE, df=NULL)
	{
  # if is.null(df), we are just searching a character vector ...
  # else, we are using the df[col] to do the search ...
    # https://stackoverflow.com/questions/5823503/
  if(is.null(df))
    {
    # col.name is a character vector
    grx = utils::glob2rx(str);
    grx.grep = grep(grx, col.name, ignore.case=ignore.case, perl=perl);
    rows = col.name[grx.grep];
    if(length(rows) == 0) { return (NA); }  # empty set ... from vector
    return (rows);
    } else
          {
          grx = utils::glob2rx(str);
          # just one column for now
          cidx = getIndexOfDataFrameColumns(df, col.name);
          if(is.na(cidx)) { cidx = -1; }
          # -1 will return an empty-set on the grep call
          grx.grep = grep(grx, df[,cidx], ignore.case=ignore.case, perl=perl);
          rows = df[grx.grep, ];
          if(is.null(return.cols)) { rows; } else { rows[, return.cols]; }
          }
  }


