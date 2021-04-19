

#' get.last.error
#'
#' @return
#' @export
get.last.error = function()
	{
  # ls(".GlobalEnv");
  # geterrmessage()
  # https://stackoverflow.com/questions/36966036/how-to-get-the-last-error
  # options(error = function() {traceback(2, max.lines=100); if(!interactive()) quit(save="no", status=1, runLast=T)})
  # https://stackoverflow.com/questions/7485514/can-you-make-r-print-more-detailed-error-messages

  tr = .traceback()  # Not a typo! .traceback is like traceback except that it doesn't force printing the stack. –
	  if(length(tr) == 0)
  	  {
  		return(NULL);
  	  }
  tryCatch(eval(parse(text = tr[[1]])), error = identity);
	}


#' parseFunctionFile
#'
#' This only works for my coding style { on new lines
#'
#' @param file
#'
#' @return
#' @export
parseFunctionFile = function(file)
	{


	lines = readLines(file);
	# we are assuming "WhiteSmith" indentation and '= function'
	left.braces = right.braces = c();
	internals = comments = c();
	fnames = c();
	functions = NULL;
	lineno = 0;
	for(line in lines)
		{
		lineno = 1 + lineno;
		line = removeWhiteSpace(line);  # this will also TRIM ...
		first = substr(line,1,1);

		if(is.substring(line, "########## INTERNAL FUNCTIONS"))
			{
			internals = c(internals, lineno);
			next;
			}

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
			functions = rbind(functions, row);

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
			functions = rbind(functions, row);

			fnames = c(fnames, lineno);
			next;
			}

		}
	# we have to line up "end of function" as well ...

	functions = as.data.frame(functions);
			# lineno.name is where the function name appears ... start is the first brace ...
		colnames(functions) = c("lineno.pre", "lineno.name", "lineno.start", "lineno.end", "fn", "parameters");
	functions = assignColumnsTypeInDataFrame(c("lineno.pre", "lineno.name", "lineno.start", "lineno.end"), "numeric", functions);

		rownames(functions) = functions$lineno.name;

	## copy ##
	lbraces = left.braces;
	rbraces = right.braces; # for debugging
	# fnames;

	# internals ... 160 200

	### braces may not be ideal if people CRAM their code together ...
	### I believe I could find a REGEX to find the matching brace immediately after function ...
	### WHAT ABOUT if PARAMETERS FLOW ACROSS MULTIPLE LINES ...

	n = nrow(functions);
	my.starts = functions$lineno.name;
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
			functions$lineno.pre[i] = current.line;
			} else 	{
					# stop("monte");
					right.braces = right.braces[-c(1:n.idx)]; # empty right.braces list
					}

		########### UPDATE PARAMETERS #############

		idx = which(left.braces > functions$lineno.name[i])[1];
		next.brace = left.braces[idx];

		functions$lineno.start[i] = next.brace;
			if( (next.brace - my.start) > 1)
				{
				idx = (my.start + 1) : (next.brace - 1);

				functions$parameters[i] = paste0(functions$parameters[i], " \n ", paste( trimMe(lines[idx]), collapse=" \n ") );

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
			functions$lineno.end[i] = my.lineno;

			current.line = 1 + my.lineno;
			right.braces = right.braces[-c(1:n.idx)]; # empty right.braces list

			} else 	{
					# last one ...

					n.b = length(right.braces);


					functions$lineno.end[i] = right.braces[n.b]; # last.brace
					# there may be extra "post" material which we ignore ... lineno from first loop ...

					}



		}
















	## let's update dataframe and skip internals ...
		## remove rows, and append "end of line" to parent ...

	## internals should be in pairs ... start/stop ...
	if(!is.null(internals))
		{
		works = cutN(internals, n=2);
		n = length(works);
		for(i in 1:n)
			{
			work = works[[i]];
			# first is which function it belongs to ...
			first = work[1];
				idx.first = which(functions$lineno.end < first);
				n.first = length(idx.first);
				# last one is correct ...
				my.fn = functions$fn[n.first];

			# last is key to getting element of last vector
			last = work[2];
				idx.last = which(functions$lineno.start < last);
				n.last = length(idx.last);
				# last one is correct ...
				my.end = functions$lineno.end[n.last];

			# update
			functions$lineno.end[n.first] = my.end;
				row.idx = (n.first+1):n.last;

			functions = functions[-c(row.idx),];


			}
		}
	functions;
	}




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



# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R')
# scanFunctionsInFile('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# functions-md5.R has "INTERNAL functions"
# don't index internals ...
indexFunctionsInFile = function(file, key="local")
	{
	search = paste0(key,"-search");
	functions = parseFunctionFile(file);
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
		str = paste0("LINES [", strPadLeft(functions$lineno.start, len.start, " ") , "-", strPadLeft(functions$lineno.end, len.end , " ") , "]", " :: ", my.file);
		n = length(fn);
	## function-level cache
	for(i in 1:n)
		{
		.GlobalEnv$.humanVerse[["functions"]][[search]][[ fn[i] ]] = str[i];
		}
	}





# punchcards had 80 characters, a traditional typewriter US had 72 characters per line (CPL)
# http://mikeyanderson.com/optimal_characters_per_line#:~:text=%E2%80%9CAnything%20from%2045%20to%2075,is%2040%20to%2050%20characters.%E2%80%9D
# Quotes "Jakob Nielson" + 5 ... states 66 is optimal
# 6.5 inches (1 inch margin) x 10 per inch ... about 65 ... we would do +/- 3 in typing class ... override end
function.summary = function(fn, key = "local", out = "cat", out.length=66, out.file="")
	{
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
	more = eval(quote(unlist(as.character(as.list(match.call()[-1])))), envir = pf);
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

  f = function(){};
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




