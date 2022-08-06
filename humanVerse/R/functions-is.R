
##################################################
#'
#' is.library
#'
#'
#' @param str (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.library = function(str = "stringi", suggestion=TRUE)
	{
	res = isTRUE(requireNamespace( str , quietly = TRUE));
	if(!res && suggestion)
		{
		pkg = paste0( "install.packages(\"",str,"\", dependencies=TRUE ); ")
		msg = paste0("\n\n", str.commentWrapper("LIBRARY is not found!"), "\n\n",
					"You could try installing the package: ", "\n\n",
					str.commentWrapper( pkg, r.tag = "-", s.pad=15), "\n");
		warning(msg);
		}
	res;
	}




##################################################
#'
#' is.substring
#'
#'
#' @param needle (substring is UNI-VARIATE)
#' @param haystack (is MULTI-VARIATE)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.substring = function(needle, haystack)
	{
	grepl(needle, haystack, fixed = TRUE);
	}



#' @rdname str_contains
#' @export
str_contains = is.substring;

#' @rdname str.contains
#' @export
str.contains = is.substring;






### THESE FUNCTIONS SEEM TO BE "mono-nuclear"
#' @rdname is.true
#' @export
is.true = isTRUE;


#' @rdname is.false
#' @export
is.false = isFALSE;



# isset — Determine if a variable is declared and is different than null

##################################################
#'
#' is.set
#'
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.set = function(obj)
	{
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
		# monte = list("1", NA_character_, jk = c(1, 243), zeroes = c(0, 0, 0, 0, 0), myNA = c(NA, NA, NA), trim = c("", "", ""), mNAN = c(NaN, NaN), mbool = c(FALSE, FALSE, FALSE)); monte$hi = NULL;
	if(isFALSE(my.obj[1])) 
		{
		e = property.get("ERROR", my.obj);
		if(e == "ERROR") { return(FALSE); }
		}
	return(TRUE);
	}


# https://www.php.net/manual/en/function.empty.php
# Determine whether a variable is empty
# Determine whether a variable is considered to be empty. A variable is considered empty if it does not exist or if its value equals false. empty() does not generate a warning if the variable does not exist
# 

##################################################
#'
#' is.empty
#'
#'
#' @param object
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.empty = function(obj)
	{
	# TEST-1 #
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
		# monte = list("1", NA_character_, jk = c(1, 243), zeroes = c(0, 0, 0, 0, 0), myNA = c(NA, NA, NA), trim = c("", "", ""), mNAN = c(NaN, NaN), mbool = c(FALSE, FALSE, FALSE)); monte$hi = NULL;
	if(is.null(my.obj)) { return(TRUE); }  
	if(isFALSE(my.obj[1])) 
		{
		e = property.get("ERROR", my.obj);
		if(e == "ERROR") { return(TRUE); }
		}

	n.len = length(my.obj);
	n.type = typeof(my.obj);

	if(n.type == "list" && n.len > 0) { return(FALSE); }


	## check for all-zeros, all-str.trim "", all-na, all-nan

	if(is.numeric(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }
	if(is.logical(my.obj) && stats.sum(my.obj) > 0) { return(FALSE); }

	# all(is.na(c(NA, NaN)))

	if(is.character(my.obj))
		{
		my.obj[is.na(my.obj)] = "";
		s1 = (my.obj == ""); # empty
		if(all(s1)) { return(TRUE); }
		}


	if(!is.null( typeof(obj) ) ) { return (FALSE); }

	return (TRUE);  # unknown typeof ?
	}



















access <- `$$` <- function(str)
	{
	E = unlist( strsplit(as.character(str),"[@]") );
		k = length(E);
		if(k==1)
			{
			eval(parse(text=str));
			} else {
				# k = 2
				nstr = paste("attributes(",E[1],")",sep="");
				nstr = paste(nstr,'$',E[2],sep="");

				if(k>2) {
					for(i in 3:k)
						{
						nstr = paste("attributes(",nstr,")",sep="");
						nstr = paste(nstr,'$',E[i],sep="");
						}
					}
				access(nstr);
				}
	}










is.empty = function(obj)
	{
	# TEST-1 #
	obj.str = deparse(substitute(obj));
	my.obj = obj.fromString(obj.str);
		# monte = list("1", NA_character_, jk = c(1, 243)); monte$hi = NULL;
		# # monte$hi4980328 is also NULL
		# # monte$nj038 is also NULL ... anything on $ because monte is list?
	if(is.null(my.obj)) { return(TRUE); }  
	if(isFALSE(my.obj[1])) 
		{
		e = property.get("ERROR");
		print(e);
		}

			# obj.fromString = function(obj.str)






print(obj.info);







	aa = tryCatch( access(obj.str), error = identity);
	
	tt = tryCatch( eval(parse(text = obj.str)), error = identity);

	# monte = list("1", NA_character_, jk = 243);
	# monte[[3334]]
	if(is.null(tt)) { return (TRUE); }

	print(obj.str);

	at = attributes(tt);

print(typeof(tt));

	if(is.list(tt) && exists("class", at))
		{
		if(at$class[2] == "error") { return (TRUE); }
		}

	print(tt);
print(at);
	str(tt);
	return(tt);

	if(!exists(obj.str)) { return (TRUE); }

	# TEST-4 #
	if(is.null(obj)) { return (TRUE); }
	# TEST-5 #
	if(length(obj) == 0 ) { return (TRUE); }

	
	n.len = length(obj);

	# TEST-6 #
	if(is.character(obj))
		{
		n.trim = str.trim(obj);
		n.es = sum(n.trim == "");
		if(n.es == n.len) { return (TRUE); }
		}

	## ALL "zero"
	
	
	# TEST-6 #
	n.nas = sum(is.na(obj));
	if(n.nas == n.len) { return (TRUE); }

	if(!is.null( typeof(obj) ) ) { return (FALSE); }

	return (FALSE);
	}








