
# ls(".GlobalEnv");
# geterrmessage()
# https://stackoverflow.com/questions/36966036/how-to-get-the-last-error
# options(error = function() {traceback(2, max.lines=100); if(!interactive()) quit(save="no", status=1, runLast=T)})
# https://stackoverflow.com/questions/7485514/can-you-make-r-print-more-detailed-error-messages

get.last.error = function()
	{
	  tr <- .traceback()  # Not a typo! .traceback is like traceback except that it doesn't force printing the stack. –
	  if(length(tr) == 0)
	  {
		return(NULL)
	  }
	  tryCatch(eval(parse(text = tr[[1]])), error = identity)
	}


parseFunctionFile = function(file)
	{
	
	
	lines = readLines(file);
	# we are assuming "WhiteSmith" indentation and '= function'
	braces = c();
	internals = c();
	functions = NULL;
	lineno = 0;
	for(line in lines)
		{
		lineno = 1 + lineno;
		line = removeWhiteSpace(line);  # this will also TRIM ...  
		
		if(line == "}")
			{
			braces = c(braces, lineno);
			next;
			}
		

		
		if(is.substring(line, "########## INTERNAL FUNCTIONS"))
			{
			internals = c(internals, lineno);
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
			row = c(NA, lineno, NA, fn,  fp);
			functions = rbind(functions, row);
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
			row = c(NA, lineno, NA, fn,  fp);
			functions = rbind(functions, row);
			}
		
		}
	# we have to line up "end of function" as well ... 
	
	functions = as.data.frame(functions);
		colnames(functions) = c("lineno.pre", "lineno.start", "lineno.end", "fn", "parameters");
	functions = assignColumnsTypeInDataFrame(c("lineno.pre", "lineno.start", "lineno.end"), "numeric", functions);

	
		rownames(functions) = functions$lineno.start;
		
		
	# cbraces = braces; # for debugging
	
	# internals ... 160 200

	### braces may not be ideal if people CRAM their code together ...
	### I believe I could find a REGEX to find the matching brace immediately after function ...
	### WHAT ABOUT if PARAMETERS FLOW ACROSS MULTIPLE LINES ...

	# braces = cbraces;
	
	n = nrow(functions);
	my.starts = functions$lineno.start;
	current.line = 1;
	
	
	for(i in 1:n)
		{
		# i = 1;
		my.start = my.starts[i];
		idx = which(braces < my.start);
		n.idx = length(idx);
		if(n.idx < 1) 
			{ 
			functions$lineno.pre[i] = current.line;
			} else 	{
					braces = braces[-c(1:n.idx)]; # empty braces list 
					}
		
		if(i < n)
			{
			next.start = my.starts[i+1];	
			idx = which(braces < next.start);  # should exist
			n.idx = length(idx);
			my.idx = idx[n.idx]; # last one 
			my.lineno = braces[my.idx];
			functions$lineno.end[i] = my.lineno;
			
			current.line = 1 + my.lineno;
			braces = braces[-c(1:n.idx)]; # empty braces list 
				
			} else 	{
					# last one ...
					
					n.b = length(braces);
					
					
					functions$lineno.end[i] = braces[n.b]; # last.brace 
					# there may be extra "post" material which we ignore ... lineno from first loop ...
					
					}
		
	
		
		}
	
	
	## let's update dataframe and skip internals ... 
		## remove rows, and append "end of line" to parent ...
		
	## internals should be in pairs ... start/stop ...
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
		
		
	
	
	functions;
	
	}
	
	
# source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-str.R')
# scanFunctionsInFile('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-get-set.R')
# functions-md5.R has "INTERNAL functions"
# don't index internals ... 
indexFunctionsInFile = function(file, key="local")
	{
	functions = parseFunctionFile(file);
	# we can store a master dataframe, or a key/value dataframe ... 
		my.file = basename(file);  				# set as key
		file.path = getDirectoryPath(file);		# set as "attribute"
	functions = setAttribute("file.path", file.path, functions);			
	
	if(!exists(my.file, .GlobalEnv$.humanVerse[["functions"]][[key]]) )
		{
		.GlobalEnv$.humanVerse[["functions"]][[key]][[my.file]] = list();
		}		
	.GlobalEnv$.humanVerse[["functions"]][[key]][[my.file]] = functions;
	
	# keyed on "fn" => `file.name.R` ...
	
	search = paste0(key,"-search");
	len.start = ceiling( log10( 1 + max(functions$lineno.start, na.rm=TRUE) ) );
	len.end   = ceiling( log10( 1 + max(functions$lineno.end, na.rm=TRUE) ) );
	
		fn = functions$fn;
		str = paste0("LINES [", strPadLeft(functions$lineno.start, len.start, " ") , "-", strPadLeft(functions$lineno.end, len.end , " ") , "]", " :: ", my.file);
		n = length(fn);
	for(i in 1:n)
		{
		.GlobalEnv$.humanVerse[["functions"]][[search]][[ fn[i] ]] = str[i];		
		}	
	}




ascii.line = function(strs, out.length=66, left = "## ", right = " ##", sep=" ", justification="center")
	{
	res = list();
	n = length(strs);
	for(i in 1:n)
		{
		str = strs[i];
	
		sep = charAt(sep,1); # we only allow a 1-element separator
		
		len.s = strlen(str);
		len.l = strlen(left);
		len.r = strlen(right);	
			
		if(justification == "center")
			{
			out.left  = out.right = floor( (out.length - len.l - len.s - len.r )/2 );
				# offset = out.length - len.l - out.left - len.s - out.right - len.r;		
			line = paste0(left, str_repeat(sep,out.left), str, str_repeat(sep, out.right));
			
			remaining = out.length - strlen(line) - len.r;			
			if(remaining > 0)
				{
				line = paste0(line, str_repeat(sep, remaining));
				}
			line = paste0(line, right);						
			} else {
					# left 
					line = paste0(left, str);
					remaining = out.length - strlen(line) - len.r;
					if(remaining > 0)
						{
						line = paste0(line, str_repeat(sep, remaining), right);
						} 
					}	
		
		res[[i]] = line;
		}
		
	if(n > 1) { res; } else { res[[1]]; }	
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
		local.start = 1 + info$more$lineno.start - info$more$lineno.pre;
	n = length(lines);
	
	preamble 	= trimMe( paste(lines[1:(local.start - 1)], collapse="\n") );
	body 		= trimMe( paste(lines[local.start:n], collapse="\n") );	
			
	info$str = list("preamble" = preamble, "body" = body);		
			
	# cat(info$str);
	
	if(out == "cat")
		{
		header = str_repeat("#", out.length);
		cat("\n", header, file=out.file, append=FALSE);
		
		line = ascii.line(fn);
		cat("\n", line, file=out.file, append=TRUE);
		
		cat("\n", header, file=out.file, append=TRUE);
		
		keys = c(" Parameters: ", " Path: ", " Source: ", " Line numbers: ", " Line count: ");
		max.key = max(strlen(keys));
		
			core = paste0( strPadLeft(keys[1], max.key, " "), df$parameters[1]);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);
		
			core = paste0( strPadLeft(keys[2], max.key, " "), info$path);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);
		
		
			core = paste0( strPadLeft(keys[3], max.key, " "), info$file);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);
		
			core = paste0( strPadLeft(keys[4], max.key, " "), info$more$lineno.start, "-", info$more$lineno.end);
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);
		
			core = paste0( strPadLeft(keys[5], max.key, " "), (1 + info$more$lineno.end - info$more$lineno.start) );
			line = ascii.line(core, justification="left");
		cat("\n", line, file=out.file, append=TRUE);
		
		
						
		
		
		if(!is.empty(preamble))
			{
			cat("\n", header, file=out.file, append=TRUE);
				line = ascii.line("PREAMBLE");
			cat("\n", line, file=out.file, append=TRUE);
			cat("\n", header, file=out.file, append=TRUE);
				lines = ascii.line( explodeMe("\n",preamble), left=" ", right=" ", justification="left");
			catMe(lines, "\n", "", file=out.file, append=TRUE);
			cat("\n", header, file=out.file, append=TRUE);
			}
			
		cat("\n", header, file=out.file, append=TRUE);
				line = ascii.line("BODY");
		cat("\n", line, file=out.file, append=TRUE);
		cat("\n", header, file=out.file, append=TRUE);
			lines = ascii.line(explodeMe("\n",body), left=" ", right=" ", justification="left");		
		catMe(lines, "\n", "", file=out.file, append=TRUE);
		cat("\n", header, file=out.file, append=TRUE);
		
		} else { info; }
	}
	
	
	

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


function.getKeyFromString = function(str)
	{
	tmp = explodeMe("::", str);
			fn.key = trimMe(tmp[2]);
	fn.key;
	}


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
	

stepIntoFunction = function(obj)
	{
	# obj is from getFunctionParameters(TRUE);
	
	
	# dput(obj);
	
	extractKeysValuesFunctionParameters(obj$.keys.      , obj$.vals.        );
	extractKeysValuesFunctionParameters(obj$.dots.keys. , obj$.dots.vals.   );	 
	}



# https://stackoverflow.com/questions/66329835/
# nice work :: B. Christian Kamgang
# .GlobalEnv$.function.args.memory ... key memory on last function call ... so I could reference outside the function
# grabFunctionParameters # 
getFunctionParameters = function(global.memory = FALSE, n=1, out.dput = FALSE)
	{
    pf			= parent.frame(n=n);
	
# arguments <- unlist( as.character( as.list( match.call( call=sys.call(1L)[[1L]]) ) ) );
	# arguments <- unlist(as.character(as.list(match.call()[-1])));
# print(arguments);	

	more = eval(quote(unlist(as.character(as.list(match.call()[-1])))), envir = pf);
# print(more);	
	
# print(pf);
    my.names	= ls(envir = pf, all.names = TRUE, sorted = FALSE);
	
# print(my.names);
	
	my.names	= ls(pos = pf, all.names = TRUE, sorted = FALSE);
	
# print(my.names);

	my.names	= ls(pos = pf, envir = pf, all.names = TRUE, sorted = FALSE);
	
# print(my.names);

# print(quote(list(...)));

	dots		= if("..." %in% my.names) { eval(quote(list(...)), envir = pf); } else { list(); }
	# dots.idx	= ( names(dots) != "" );
	
	names(dots) = more;
cat("\n\n === DOTS === \n\n");
print(dots);
cat("\n\n === NAMES DOTS === \n\n");
print( names(dots) );
# cat("\n\n === DOTS.IDX === \n\n");
# print(dots.idx);
    remaining 	= sapply( setdiff(my.names, "..."), as.name);
# cat("\n\n === remaining === \n\n");
# print(remaining);
	not.dots	= if(length(remaining) > 0) { lapply( remaining, eval, envir = pf);  } else { list(); }
# cat("\n\n === not.dots === \n\n");
# print(not.dots);

	res = list();

		res$.fn. 			= as.character( sys.call(1L)[[1L]] );
		# res$.scope. 		= pf;
		# dput doesn't like scope 
		res$.keys. 			= names( not.dots );
		res$.vals. 			= not.dots; 							# unname(not_dots);  # I want keys on "vals"
		res$.dots.keys. 	= names(dots); 							# names( dots[dots.idx] );
		res$.dots.vals. 	= dots;									# dots[dots.idx]; 						# unname(dots[dots.idx]);

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

	res;
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




