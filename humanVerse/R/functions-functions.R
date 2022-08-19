
# setwd("C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R")


# include.dir = source.dir ... if is local ? otherwise, I need a parser ... github
# USE dir.exists ........... is.dir ... ?local in help on SOURCE is different thing
# include.url = source.url
# include = source.local ... this indexes 

# # ?source ... sourceDir
## cat("\n", res$myerrors[ res$myerrors != ""], sep="\n" );

## res = include.dir(getwd()); View(res); res$myerrors[ res$myerrors != ""]

include.dir = function() {}
include.dir = function(path = getwd(), verbose = TRUE, pattern = "[.][RrSsQq]$")
	{
debug = FALSE;
	op = options(); on.exit(options(op)); # to reset after each 
	myfiles = list.files(path, pattern = pattern);
	n = length(myfiles);
	if(verbose) { cat("\n ATTEMPTING TO INCLUDE ", n, " files ... ", "\n"); }
	j = NULL;
	# myerrors = list();
	myerrors = character(n);
	myfullpaths = character(n);
	for(i in 1:n)
		{
		myfile = myfiles[i];
		if(verbose) { cat(myfile,": "); }
		# this should go in `include.local` ... 
		# you can't USE `dots` inside suppressError ... maybe wrap in TRY/CaTCH
		myfullpaths[i] = file.path(path, myfile);
		# w = suppressError( source( myfullpaths[i], ...), 
		#			show.notice=debug, msg="debugging source.dir" );
					
					
		w = suppressError( source( myfullpaths[i] ), 
					show.notice=debug, msg="debugging include.dir" );
					
		if( is.error(w) ) 
			{ 
			w = as.character(w);			
			j = c(j, i); 			
			myerrors[i] = w;
			if(verbose)
				{
				cat("\n\n", w, "\n\n"); 
				}
			} else { if(verbose) { cat("\n"); } }
		
		# so maybe the SOURCE is not just functions, but CODE
		# changing the options 
		options(op);
		}
		
	df = data.frame( cbind(myfiles, myerrors, myfullpaths) );
	if(verbose) 
		{ 
		cat("\n Reporting ", length(j), " errors on ", n, " includes ... ", "\n"); 
		print(df);
		cat("\n Reporting ", length(j), " errors on ", n, " includes ... ", "\n");
		print(df$myerrors[ df$myerrors != ""]);
		}	
	invisible(df);
	}


	






	
functions.whereIs = function(fn = "base:::curlDownload")
	{
	# how to cast as character if it is the fn obj ...
	# 
	
	}

# https://www.youtube.com/watch?v=jZY4zR8_7eI
# functions as mapping ... abstract algebra


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#'
#' functions.cleanKey
#'
#' This INTENTIONALLY uses internal base R to prevent 'recursion'
#'
#' @param key (what is the character string to be keyed)
#' @param n=1 (how many characters of key to return)
#'
#' @return Updated key
#' @export
#'
#' @examples
functions.cleanKey = function() {}
functions.cleanKey = function(key, n=1, keep="", extra = "! #")
	{
	str = tolower(key); # has to be base-R (not str.tolower, recursion)
	if(extra == "")
		{
		# recursion, these functions are calling cleanup 
		# Error: node stack overflow
		# Error: no more error handlers available (recursive errors?); invoking 'abort' restart
		n = nchar(extra);  # nchars 
		res = str;
		for(i in 1:n)
			{
			res = gsub(strsplit(extra, "")[[1]], "", res, fixed=TRUE);
			}
		str = res;
		}
	if(keep != "")
		{
		tmp = strsplit(str, keep, fixed=TRUE)[[1]];
		res = paste0( substring(tmp, 1, n), collapse=keep);
		return(res);
		}
		
	# we could explode(extra[i]).implode("") ## REMOVE
	# if keep, explode("-"), return n elements 
	# separated by keep ([f]irst-[s]econd-[t]hird]) ... f-s-t
	substr(str,1,n);
	} 


#' @rdname functions.cleanupKey
#' @export
functions.cleanupKey = functions.cleanKey;

#' @rdname functions.cleanUpKey
#' @export
functions.cleanUpKey = functions.cleanKey;


# gets the RAW function ... 
# property.get("srcref", ping.domain)
# compare to body(ping.domain)
# ?? Rprof ... turn profiling on/off ... 
# Rprof("boot.out")
# storm.boot <- boot(rs, storm.bf, R = 4999) # slow enough to profile
# Rprof(NULL)

	
functions.getParameterInfo = function(return="dots", truncate=10)
	{
	pf = parent.frame(1);
	fn = as.character(sys.call(1L)[[1L]]);
	arg.names = ls(envir = pf, all.names = TRUE, sorted = FALSE);
	
	dots = list();
	if("..." %in% arg.names) 
		{ 
		dots = eval(quote(list(...)), envir = pf); 
		}
	dots = list.truncateLength(dots, truncate);
	
	# remove dots, remaining is main
	arg.names = sapply(setdiff(arg.names, "..."), as.name)
	
	main = list();
	if(length(arg.names)) 
		{
		main = lapply(arg.names, eval, envir = pf)
		}
	main = list.truncateLength(main, truncate);
	
	r = functions.cleanKey(return, 1);
	if(r == "f") { return(fn); }
	if(r == "p") { return(pf); }
	if(r == "m") { return(main); }
	if(r == "d") { return(dots); }
	# fallback is ALL
	list("fn" = fn, "pf" = pf, "main" = main, "dots" = dots);
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	






























function.arguments <- function() {
    # https://stackoverflow.com/questions/66329835/
    # nice work :: B. Christian Kamgang
    # .GlobalEnv$.function.args.memory ... key memory on last function call ... so I could reference outside the function
    # grabFunctionParameters #
    pf <- parent.frame()
    args_names <- ls(envir = pf, all.names = TRUE, sorted = FALSE)
    if("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = pf)
    }  else {
    dots = list()
    }
    args_names <- sapply(setdiff(args_names, "..."), as.name)
    if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = pf)
    } else {
    not_dots <- list()
    }
dput(dots);
   idx <- names(dots) != "";
   list(.keys. = names(not_dots), 
   .vals. = unname(not_dots), 
   .fn. = as.character(sys.call(1L)[[1L]]), 
   .scope. = pf, 
   .dot.keys. = names(dots[idx]), 
   .dot.vals. = unname(dots[idx]));
}



 

# so we can "step into" function for debugging
functions.setDefaultValues = function(fn, ...)
	{
	if(is.character(fn)) { fn = str.toObject(fn); }
	# not working in RSTUDIO, CLI ... why ?
	list.extract( formals(fn), ... ); # by default into GLOBAL
	}

functions.stepInto = functions.setDefaultValues

functions.trapErrors = function(expression)
	{


	}















#' sourceMe
#'
#' @param myfile
#' @param key
#' @param indexFunctions
#'
#' @return
#' @export
#'
#' @examples
sourceMe = function(myfile, key = "local", indexFunctions = TRUE)
	{
  # source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# mySource('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')

	if(!indexFunctions)
		{
		source(myfile);
		} else  {
				indexFunctionsInFile(myfile, key=key, include=TRUE);
				# include will "source(myfile)"
				# this will store to cache
				# source(myfile);
				}
	}


#' parseFunctionFile
#'
#' This only works for my coding style { on new lines
#'
#' @param file
#'
#' @return
#' @export
parseFunctionFile = function(file, verbose=FALSE)
	{
  internal.start  = "############### .INTERNAL FUNCTIONS";
  internal.end    = "############### INTERNAL FUNCTIONS.";


	lines = readLines(file);
	# we are assuming "WhiteSmith" indentation and '= function'
	left.braces = right.braces = c();
	internals = comments = c();
	search.internals = FALSE;
	# internals start/stop?
	fnames = c();
	myfns = NULL;
	lineno = 0;
	for(line in lines)
		{
		lineno = 1 + lineno;
		line = removeWhiteSpace(line);  # this will also TRIM ...
		first = substr(line,1,1);

		if(verbose) { cat("\n \t lineno: ", lineno, "\n"); }

		# if(is.substring(line, "########## INTERNAL FUNCTIONS"))
		# 	{
		# 	internals = c(internals, lineno);
		# 	next;
		#   }

		if(is.substring(line, internal.start))
			{

			search.internals = TRUE;
			if(verbose) { cat("\n ", internal.start, "\n"); }
			internals = c(internals, lineno);
			next;
		  }

		if(is.substring(line, internal.end))
			{
		  search.internals = FALSE;
		  if(verbose) { cat("\n ", internal.end, "\n"); }
			internals = c(internals, lineno);
			next;
			}

		if(search.internals == TRUE) { next; } # let's just skip until we find end

		if(first == "#")
			{
			# comment
			comments = c(comments, lineno);
			next;
			}



		if(first == "{")
			{
			left.braces = c(left.braces, lineno);
			next;
			}

		if(first == "}")
			{
			right.braces = c(right.braces, lineno);
			next;
			}





		### whitesmith form
		grx = utils::glob2rx("*= function*");  # could add same syntax for "<- function"
		grx.grep = grep(grx, line);
		if(length(grx.grep) > 0)
			{
			# new function is here ...
				tmp = explodeMe("=", line);
				n.tmp = length(tmp);
				fn = trimMe(tmp[1]);
				fp = trimMe( str_replace("function", "", paste(tmp[2:n.tmp], collapse="=") ) );
			row = c(NA, lineno, NA, NA, fn,  fp);
			myfns = rbind(myfns, row);

			fnames = c(fnames, lineno);
			next;
			}

		### "other" non-C based "assignment" form
		grx = utils::glob2rx("*<- function*");  # could add same syntax for "<- function"
		grx.grep = grep(grx, line);
		if(length(grx.grep) > 0)
			{
			# new function is here ...
				tmp = explodeMe("=", line);
				n.tmp = length(tmp);
				fn = trimMe(tmp[1]);
				fp = trimMe( str_replace("function", "", paste(tmp[2:n.tmp], collapse="=") ) );
			row = c(NA, lineno, NA, NA, fn,  fp);
			myfns = rbind(myfns, row);

			fnames = c(fnames, lineno);
			next;
			}

		}
	# we have to line up "end of function" as well ...

	myfns = as.data.frame(myfns);
	n = nrow(myfns);
	cat("\n \t", file, " has [",n,"] functions. \n");




	if(n < 1) { return(NULL); }
	if(n > 0)
	  {
			# lineno.name is where the function name appears ... start is the first brace ...
		colnames(myfns) = c("lineno.pre", "lineno.name", "lineno.start", "lineno.end", "fn", "parameters");
	myfns = assignColumnsTypeInDataFrame(c("lineno.pre", "lineno.name", "lineno.start", "lineno.end"), "numeric", myfns);

		rownames(myfns) = myfns$lineno.name;

		# print(myfns);

	## copy ##
	lbraces = left.braces;
	rbraces = right.braces; # for debugging
	# fnames;

	# internals ... 160 200

	### braces may not be ideal if people CRAM their code together ...
	### I believe I could find a REGEX to find the matching brace immediately after function ...
	### WHAT ABOUT if PARAMETERS FLOW ACROSS MULTIPLE LINES ...


	my.starts = myfns$lineno.name;
	current.line = 1;

	for(i in 1:n)
		{
		# i = 1;
		my.start = my.starts[i];
		idx = which(right.braces < my.start);
		n.idx = length(idx);
		########### GET PREAMBLE #############
		if(n.idx < 1)
			{
			myfns$lineno.pre[i] = current.line;
			} else 	{
					# stop("monte");
					right.braces = right.braces[-c(1:n.idx)]; # empty right.braces list
					}

		########### UPDATE PARAMETERS #############

		idx = which(left.braces > myfns$lineno.name[i])[1];
		next.brace = left.braces[idx];

		myfns$lineno.start[i] = next.brace;
			if( (next.brace - my.start) > 1)
				{
				idx = (my.start + 1) : (next.brace - 1);

				myfns$parameters[i] = paste0(myfns$parameters[i], " \n ", paste( trimMe(lines[idx]), collapse=" \n ") );

				# stop("monte");
				}
		# empty left.braces
			idx = which(left.braces <= next.brace);  # should exist
			n.idx = length(idx);
				left.braces = left.braces[-c(1:n.idx)]; # empty right.braces list

		########### GET BODY #############
		if(i < n)
			{
			next.start = my.starts[i+1];
			idx = which(right.braces < next.start);  # should exist
			n.idx = length(idx);
			my.idx = idx[n.idx]; # last one
			my.lineno = right.braces[my.idx];
			if(length(my.lineno) > 0)
			{
			myfns$lineno.end[i] = my.lineno;

			current.line = 1 + my.lineno;
			}

			right.braces = right.braces[-c(1:n.idx)]; # empty right.braces list

			} else 	{
					# last one ...

					n.b = length(right.braces);


					myfns$lineno.end[i] = right.braces[n.b]; # last.brace
					# there may be extra "post" material which we ignore ... lineno from first loop ...

					}

		}


	}


	## let's update dataframe and skip internals ...
		## remove rows, and append "end of line" to parent ...

	## DOES THIS WORK, HIDE in BODY OF PARENT FUNCTION
	## internals should be in pairs ... start/stop ...
	if(!is.null(internals))
		{
		works = cutN(internals, n=2);
		m = length(works);
		for(i in 1:m)
			{
			work = works[[i]];
			# first is which function it belongs to ...
			first = work[1];
				idx.first = which(myfns$lineno.end < first);
				n.first = length(idx.first);
				# last one is correct ...
				my.fn = myfns$fn[n.first];

			# last is key to getting element of last vector
			last = work[2];
				idx.last = which(myfns$lineno.start < last);
				n.last = length(idx.last);
				# last one is correct ...
				my.end = myfns$lineno.end[n.last];

			# update
			if(length(my.end) > 0)
			{
			myfns$lineno.end[n.first] = my.end;
			}
				row.idx = (n.first+1):n.last;

			## myfns = myfns[-c(row.idx),];


			}
	  }


	myfns;
	}




#' indexFunctionsInAttachedPackages
#'
#' @param packages
#'
#' @return
#' @export
#'
#' @examples
indexFunctionsInAttachedPackages = function(packages = "ALL")
	{
	# RStudio inflates the search list ...
  # Rgui # basic after running SIMULATIONS
# > search()
 # [1] ".GlobalEnv"            "package:pracma"        "package:PolynomF"
 # [4] "package:humanVerseWSU" "package:stats"         "package:graphics"
 # [7] "package:grDevices"     "package:utils"         "package:datasets"
# [10] "package:methods"       "Autoloads"             "package:base"

# ls("package:stats")
# lsf.str("package:stats")
# x <- library(help = stats)
# x$info[[2]]


	}

#' indexFunctionsInFolder
#'
#' @param folder
#' @param key
#' @param include
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
indexFunctionsInFolder = function(folder, key="local", include=TRUE, pattern = "[.][RrSsQq]$")
	{
  # folder = 'C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/';

	files = list.files(folder, pattern=pattern);
	for(file in files)
		{
	  # print(file);
	  # if(file == "functions-functions.R") { next; }
		indexFunctionsInFile(file, key=key, include=include);
		}
	}


#' getFunctionStringFromFile
#'
#' @param file
#' @param myfn
#'
#' @return
#' @export
#'
#' @examples
getFunctionStringFromFile = function(file, myfn)
		{
		lines 		= readLines(file);
		myfns 	= parseFunctionFile(file);
				fn 	= myfns$fn;
		idx 		= which(fn == myfn);

		if(length(idx) < 1) { message.stop("Function: ", myfn, " \n\t not found in file: ", file, " \n\t\t from function: getFunctionStringFromFile"); }

		row = myfns[ idx[1], ];
			row.start 	= row$lineno.name;
			row.end 	= row$lineno.end;

		str = paste0( paste( lines[row.start:row.end], collapse="\n"), "\n");
		# cat(str);
		# cat(gettext(str)); # what ???
		str;
		}



#' indexFunctionsInFile
#'
#' @param file
#' @param key
#' @param include
#'
#' @return
#' @export
#'
#' @examples
indexFunctionsInFile = function(file, key="local", include=TRUE)
	{
  # source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R')
# scanFunctionsInFile('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# functions-md5.R has "INTERNAL functions"
# don't index internals ...

	search = paste0(key,"-search");
	functions = parseFunctionFile(file);

	if(!is.null(functions))
	{
	# we can store a master dataframe, or a key/value dataframe ...
		my.file = basename(file);  				# set as key
		file.path = getDirectoryPath(file);		# set as "attribute"
	functions = setAttribute("file.path", file.path, functions);

	if(!exists(my.file, .GlobalEnv$.humanVerse[["functions"]][[key]]) )
		{
		.GlobalEnv$.humanVerse[["functions"]][[key]][[my.file]] = list();
		}
	## file-level cache
	.GlobalEnv$.humanVerse[["functions"]][[key]][[my.file]] = functions;

	# keyed on "fn" => `file.name.R` ...


	len.start = ceiling( log10( 1 + max(functions$lineno.start, na.rm=TRUE) ) );
	len.end   = ceiling( log10( 1 + max(functions$lineno.end, na.rm=TRUE) ) );

		fn = functions$fn;
		str = paste0("BODY [",
		        strPadLeft(functions$lineno.start, len.start, " ") , "-",
		        strPadLeft(functions$lineno.end, len.end , " ") , "]", " :: ",
		        my.file, " :: ", functions$fn, "() ",
		        functions$lineno.name,":",functions$lineno.end );
		n = length(fn);
	## function-level cache
	for(i in 1:n)
		{
		.GlobalEnv$.humanVerse[["functions"]][[search]][[ fn[i] ]] = str[i];
		}

	if(include)
		{
		source(my.file);
	  }

	}

	}





#' function.summary
#'
#' @param fn
#' @param key
#' @param out
#' @param out.length
#' @param out.file
#'
#' @return
#' @export
#'
#' @examples
function.summary = function(fn, key = "local", out = "cat", out.length=66, out.file="")
	{
  # punchcards had 80 characters, a traditional typewriter US had 72 characters per line (CPL)
# http://mikeyanderson.com/optimal_characters_per_line#:~:text=%E2%80%9CAnything%20from%2045%20to%2075,is%2040%20to%2050%20characters.%E2%80%9D
# Quotes "Jakob Nielson" + 5 ... states 66 is optimal
# 6.5 inches (1 inch margin) x 10 per inch ... about 65 ... we would do +/- 3 in typing class ... override end

	fn = trimMe(fn);
	info = list();
	info$fn = fn;

	search = paste0(key,"-search");

		res = .GlobalEnv$.humanVerse[["functions"]][[search]][[ fn ]];
		if(is.null(res)) { return("NA"); }
	info$file 	= fn.key = function.getKeyFromString(res);
	info$path = getAttribute("file.path", .GlobalEnv$.humanVerse[["functions"]][[key]][[fn.key]]);
	info$basic 	= res;

		df = .GlobalEnv$.humanVerse[["functions"]][[key]][[fn.key]];
		df = subsetDataFrame(df, "fn", "==", fn);
	info$more = df;
	mystr = file.readLines( paste0(info$path, info$file), n=df$lineno.end, skip = df$lineno.pre );



	lines = explodeMe("\n", mystr);
		local.start = 1 + info$more$lineno.name - info$more$lineno.pre;
	n = length(lines);

	#print(info$more);
	#print(lines);

	preamble 	= trimMe( paste(lines[1:(local.start - 1)], collapse="\n") );
		body.skip = info$more$lineno.start - info$more$lineno.name;
	body 		= trimMe( paste(lines[(local.start + body.skip):n], collapse="\n") );

	info$str = list("preamble" = preamble, "body" = body);


	keys = c(" Parameters: ", " Path: ", " Source: ", " Line numbers: ", " Line count: ");
		max.key = max(strlen(keys));


	# cat(info$str);

	if(out == "cat")
		{
		header = str_repeat("#", out.length);
		subheader = paste0("##", str_repeat("-", (out.length-4) ), "##");
		cat("\n", header, file=out.file, append=FALSE);

		line = ascii.line(fn);
		cat("\n", line, file=out.file, append=TRUE);

		cat("\n", subheader, file=out.file, append=TRUE);





		########## PATH ##########
			core = paste0( strPadLeft(keys[2], max.key, " "), info$path);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);



			core = paste0( strPadLeft(keys[3], max.key, " "), info$file);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);

		cat("\n", subheader, file=out.file, append=TRUE);

		########## LINE NOS ##########

			core = paste0( strPadLeft(keys[4], max.key, " "), info$more$lineno.start, "-", info$more$lineno.end);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);

			core = paste0( strPadLeft(keys[5], max.key, " "), (1 + info$more$lineno.end - info$more$lineno.start) );
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);


		cat("\n", subheader, file=out.file, append=TRUE);

		########## PARAMETERS ##########
				parameters = explodeMe("\n", df$parameters[1]);
				n.p = length(parameters);
			core = paste0( strPadLeft(keys[1], max.key, " "), parameters[1]);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);   # empty out.file will print to screen ...
				if(n.p > 1)
					{
					lines = ascii.line( parameters[2:n.p], left="## \t\t\t ", right=" ", justification="left");
					catMe(lines, "\n", "", file=out.file, append=TRUE);
					}



		cat("\n", header, file=out.file, append=TRUE);
				line = ascii.line("BODY");
		cat("\n", line, file=out.file, append=TRUE);
		cat("\n", subheader, file=out.file, append=TRUE);
				bodylines = explodeMe("\n",body);
				bodylines[1] = paste0("\t", bodylines[1]); # putting TAB back
			lines = ascii.line(bodylines, left=" ", right=" ", justification="left");
		catMe(lines, "\n", "", file=out.file, append=TRUE);
		cat("\n", subheader, file=out.file, append=TRUE);



		if(!is.empty(preamble))
			{

				line = ascii.line("PREAMBLE");
			cat("\n", line, file=out.file, append=TRUE);
			cat("\n", header, file=out.file, append=TRUE);
				lines = ascii.line( explodeMe("\n",preamble), left="### ", right=" ", justification="left");
			catMe(lines, "\n", "", file=out.file, append=TRUE);
			cat("\n", subheader, file=out.file, append=TRUE);
			}


		} else { info; }
	}



#' function.getKeyFromString
#'
#' Used in 'function.summary' ... would be nice to have 'traceforward()' function
#'
#' @param str
#'
#' @return
#' @export
function.getKeyFromString = function(str)
	{
	tmp = explodeMe("::", str);
			fn.key = trimMe(tmp[2]);
	fn.key;
	}


# MASS\R\misc.R has the ginv function ... would be nice to load library, build -ONE- and then read -ONE- that reminds filename/line numbers
#' function.whereIs
#'
#' Sometimes we include lots of functions in lots of files.
#' If registered with sourceMe ('indexFunctionsInFile'), we can see where.
#'
#' @param fns
#' @param ...
#' @param key
#' @param getFullPath
#'
#' @return
#' @export
function.whereIs = function(fns, ...,  key = "local", getFullPath=TRUE)
	{

	more = unlist(list(...));
	fns = c(fns, more);

	# we have it cached under a key ...
	search = paste0(key,"-search");

	mylen = max(strlen(fns));

	result = c();
	for(fn in fns)
		{
		fn.str = strPadLeft(fn, mylen, padding=" ");
		res = .GlobalEnv$.humanVerse[["functions"]][[search]][[ fn ]];
		if(!is.null(res) && getFullPath)
			{
			fn.key = function.getKeyFromString(res);
			res = paste0(fn.str, " ... ", res, " (at) ", getAttribute("file.path", .GlobalEnv$.humanVerse[["functions"]][[key]][[fn.key]]) );
			} else 	{
					res = paste0(fn.str, " ... ", res );
					}
		if(is.null(res)) { res = "NA"; }

		result = c(result, res);
		}
	result;
	}




#' extractKeysValuesFunctionParameters
#'
#' Globalizes variables for 'stepIntoFunction'
#'
#' @param Keys
#' @param Vals
#' @param envir
#'
#' @return
#' @export
extractKeysValuesFunctionParameters = function(Keys, Vals, envir = .GlobalEnv)
    {
    n = length(Vals); # need to be same length
  	if(n > 0)
  		{
  		for(i in 1:n)
  			{
  			myKey = Keys[i];
  			myVal = Vals[[myKey]];
  			assign(myKey, myVal, pos = envir);
  			}
  		}
    }




#' stepIntoFunction
#'
#' The idea is to call this function so you don't have to
#' prepopulate default parameters.
#'
#' @param obj
#'
#' @return
#' @export
stepIntoFunction = function(obj)
	{

	# obj is from getFunctionParameters(TRUE);


	# dput(obj);

	extractKeysValuesFunctionParameters(obj$.keys.      , obj$.vals.        );
	extractKeysValuesFunctionParameters(obj$.dots.keys. , obj$.dots.vals.   );
	}




#' grabFunctionParameters
#'
#' @return
#' @export
grabFunctionParameters <- function() {
    # https://stackoverflow.com/questions/66329835/
    # nice work :: B. Christian Kamgang
    # .GlobalEnv$.function.args.memory ... key memory on last function call ... so I could reference outside the function
    # grabFunctionParameters #
    pf <- parent.frame()
    args_names <- ls(envir = pf, all.names = TRUE, sorted = FALSE)
    if("..." %in% args_names) {
    dots <- eval(quote(list(...)), envir = pf)
    }  else {
    dots = list()
    }
    args_names <- sapply(setdiff(args_names, "..."), as.name)
    if(length(args_names)) {
    not_dots <- lapply(args_names, eval, envir = pf)
    } else {
    not_dots <- list()
    }
   idx <- names(dots) != "";
   list(.keys. = names(not_dots), .vals. = unname(not_dots), .fn. = as.character(sys.call(1L)[[1L]]), .scope. = pf, .dot.keys. = names(dots[idx]), .dot.vals. = unname(dots[idx]));
}



# grabs all functions in BODY of function, not if/else AWARE
# add library and base functions (loaded) 
# ls ?
# function.traceforward
traceforward = function(f.str = "shell", max.depth=10)
  {
  f.res = list();
  f.res[[1]] = f.todo = f.list = registered.functions.in.function(f.str);
  depth = 1;
  while(length(f.todo) > 0) # this is "recursive" ?
    {
    if(depth >= max.depth) { break; }
    depth = 1 + depth;
    f.res[[depth]] = list();
    cat("\n\t depth: ", depth, "\n");
    for(fn.str in names(f.todo))
      {
      f.new = registered.functions.in.function(fn.str);
      f.todo[[fn.str]] = NULL;
      for(fnew.str in names(f.new))
        {
        if(!exists(fnew.str, f.list))
          {
          f.list[[fnew.str]] = 1;
          f.todo[[fnew.str]] = 1;
          f.res[[depth]][[fnew.str]] = 1;
          }
        }
      }
    }

  f.list;
  }



find.functions.in.string = function(fn.body)
  {
  f.list = list();
  fn.tmp = explodeMe("(",fn.body);

	for(fn.tm in fn.tmp)
		{
		fn.tm = trimMe(str_replace("\n", " ", fn.tm));
		fn.ex = explodeMe(" ",fn.tm);
		fn.len = length(fn.ex);
		fn.w = fn.ex[fn.len]; # last word

		# print(fn.w);
# does this work for base::is.na type functions?
		is.fun = try( is.function( eval(parse(text=fn.w)) ), silent=TRUE);

		if(isTRUE(is.fun))
			{
			# print("YES");
			if(exists(fn.w, f.list))
				{
				f.list[[fn.w]] = 1 + f.list[[fn.w]];
				} else {f.list[[fn.w]] = 1; }
			}
		}



	# is.function( eval(parse(text="chartr")) );
	# is.function( eval(parse(text="if")) );


	# maybe sort list alphabetically or based on counts... currently it is the order they appeared
	# maybe cast as dataframe to make this easier to do
	f.list;

  }


#' registered.functions.in.function
#'
#' @param f.str
#' @param f.list
#' @param sort
#'
#' @return
#' @export
#'
#' @examples
registered.functions.in.function = function(f.str = "shell")
	{
  # see "basename, we lose ".Internal" because of "body" ...

	fn = eval(parse(text=f.str));
	fn.body = paste0( paste(as.character(body(fn)), collapse="\n"), "\n");

	find.functions.in.string(fn.body);
	}





#' pseudoCompile
#'
#' @param srcpaths
#' @param outfile
#' @param append.where
#'
#' @return
#' @export
#'
#' @examples
pseudoCompile = function(srcpaths, outfile, key = "local-search",
                          append.start="##### .INCLUDE #####",
                          append.end="##### INCLUDE. #####")
  {
  # this will find all necessary functions from srcpaths
  # this will append them between ### ALEX ### in outfile
  # thereby including them before necessary call
  # after ### ALEX ### we will look for functions,
    # if it starts on a new line, no indents, it is a function (

  # multi-sourcs
  # multi-functions

  outpath = getDirectoryPath(outfile);
  haystack = readStringFromFile(outfile);

  first = trimMe(explodeMe(append.start, haystack)[1]);
    tmp = explodeMe(append.end, haystack);
    nt = length(tmp);
  last = trimMe(tmp[nt]);

  for(srcpath in srcpaths)
    {
    # let's load everything, so it register
    indexFunctionsInFolder(srcpath);
    }

  # review last for functions ... AFTER REGISTRATION
  f.list = find.functions.in.string(last);
  if(length(f.list) < 1) { stop("error"); }

  search = .GlobalEnv$.humanVerse[["functions"]][[key]];

  includes = list();
  for(fn in names(f.list))
    {
    tf = traceforward(fn);
    for(tn in names(tf))
      {
      if(exists(tn, search))
        {
        includes[[tn]] = 1;
        }
      }
    }


  istr = "\n";
  for(fn in names(includes))
    {
    line = .GlobalEnv$.humanVerse[["functions"]][[key]][[fn]];
      tmp = explodeMe("::",line);
      cfiles = paste0(srcpaths, trimMe(tmp[2]) );  # append to src paths?
      for(cfile in cfiles)
        {
        if(file.exists(cfile)) { break; }
        }
      tmp2 = explodeMe(")", tmp[3]);
      tmp3 = as.numeric( trimMe( explodeMe(":", tmp2[2]) ) );
    # lstr = paste0("## ", basename(cfile),"\n", file.readLines(cfile, tmp3[2], (tmp3[1]) ) );
    lstr = paste0("## ", cfile,"\n", file.readLines(cfile, tmp3[2], (tmp3[1]) ) );

      # cat(lstr);
    istr = paste0( istr, lstr, "\n");
    }

  final.str = paste0(first, "\n\n", append.start, istr, "\n", append.end, "\n\n", last);

  writeLine(final.str, outfile, append=FALSE);
  }




#' getFunctionParameters
#'
#' TODO: recognize ... elements based on their names ...
#'
#' @param global.memory If TRUE, you can access values outside function (later)
#' @param n
#' @param out.dput
#'
#' @return
#' @export
getFunctionParameters = function(global.memory = TRUE, n=1, out.dput = FALSE)
	{
    pf			= parent.frame(n=n);

						### arguments <- unlist( as.character( as.list( match.call( call=sys.call(1L)[[1L]]) ) ) );
						########## arguments <- unlist(as.character(as.list(match.call()[-1])));
						########## print(arguments);
## cat("\n\n === MORE === \n\n");
	# Error in match.call() : ... used in a situation where it does not exist
	mc = match.call()[-1];
#	print(mc);
	more = NULL;
	if(!is.null(mc))
		{
		# exists("mc") ??? 
		if(is.list(mc))
			{
			more = eval(quote(unlist(as.character(as.list( mc )))), envir = pf);
			}
		}

##  print(more);

## cat("\n\n === PF === \n\n");
##  print(pf);
##     my.names	= ls(envir = pf, all.names = TRUE, sorted = FALSE);

## cat("\n\n === NAMES-1 === \n\n");
##  print(my.names);

## 	my.names	= ls(pos = pf, all.names = TRUE, sorted = FALSE);

## cat("\n\n === NAMES-2 === \n\n");
##  print(my.names);

	my.names	= ls(pos = pf, envir = pf, all.names = TRUE, sorted = FALSE);

## cat("\n\n === NAMES-3 === \n\n");
 ## print(my.names);

## cat("\n\n === LIST === \n\n");
 ## print(quote(list(...)));

	dots		= if("..." %in% my.names) { eval(quote(list(...)), envir = pf); } else { list(); }
## cat("\n\n === DOTS === \n\n");
## print(dots);

 dots.idx	= ( names(dots) != "" );
## cat("\n\n === DOTS-IDX === \n\n");
## print(dots);



## cat("\n\n === NAMES DOTS === \n\n");
## print( names(dots) );
##  cat("\n\n === DOTS.IDX === \n\n");
##  print(dots.idx);
    remaining 	= sapply( setdiff(my.names, "..."), as.name);
##  cat("\n\n === remaining === \n\n");
##  print(remaining);
	not.dots	= if(length(remaining) > 0) { lapply( remaining, eval, envir = pf);  } else { list(); }
 ## cat("\n\n === not.dots === \n\n");
 ## print(not.dots);

	res = list();

 ## cat("\n\n === FINAL-RES === \n\n");
		res$.fn. 			= as.character( sys.call(1L)[[1L]] );
		# res$.scope. 		= pf;
		# dput doesn't like scope
		res$.keys. 			= names( not.dots );
		res$.vals. 			= not.dots; 							# unname(not_dots);  # I want keys on "vals"
		res$.dots.keys. 	= names(dots); 							# names( dots[dots.idx] );
		res$.dots.vals. 	= dots;									# dots[dots.idx]; 						# unname(dots[dots.idx]);

## print(res);

 ## cat("\n\n === FINAL-RES-DPUT === \n\n");

if(out.dput)
	{
	dput(res);
	}

	# we will store in stack ... just last call to this function
	if(global.memory)
		{
		fn = res$.fn.;
		.GlobalEnv$.humanVerse[["stack"]][[ fn ]] = res;

		n = length(.GlobalEnv$.humanVerse[["stack-order"]]) + 1;
		.GlobalEnv$.humanVerse[["stack-order"]][[n]] = fn;

		# if(n > .GlobalEnv$.humanVerse[["system"]][["stack-length"]])
			{
			# cat("\n", "NEED TO POP STACK", "\n");
			}
		}



	## cat("\n\n === FINAL-END === \n\n");
	return(res);
	}




#' castStringAsFunction
#'
#' @param fstr The RHS (right hand side) of a function in string form.
#' @param ... The elements in RHS that are parameters (e.g., x)
#' @param envir The scope of the environment for the function
#'
#' @return A function
#' @export
#'
#' @examples
#' x = -3:3;
#' FUN = "exp( 3 * x^2 + 2 * x + 1)";
#' myFunction = castStringAsFunction (  FUN, x );
#' myFunction;
#' myFunction(x);
#'
castStringAsFunction = function(fstr, ..., envir = parent.frame() )
  {
  # https://stackoverflow.com/questions/66266860/
  dots            = match.call(expand.dots = FALSE)$... ;
  form_ls         = rep(list(bquote()), length(dots));
  names(form_ls)  = as.character(dots);
############### .INTERNAL FUNCTIONS ###############
  f = function()
    {
    }
############### INTERNAL FUNCTIONS. ###############

    formals(f)      = form_ls;
    body(f)         = str2lang(fstr);
    environment(f)  = envir;

  f;
  }


# R> library(fortunes)
# R> fortune("parse")

# If the answer is parse() you should usually rethink the question.
   # -- Thomas Lumley
      # R-help (February 2005)

# R>


