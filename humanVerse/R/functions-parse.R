


#' cleanup.local
#'
#' @param myfile The original filename, may be a URL with ? = & symbols
#'
#' @return A windoze-friendly filename
#' @export
#'
#' @examples
#' cleanup.local("index.html?endYear=1920&amount=1000000");
#'
cleanup.local = function(myfile, append="")
	{
	myfile = str_replace("//", "/",   myfile);
	myfile = str_replace("?", "^-QUESTION-^",   myfile);
	myfile = str_replace("&", "^-AND-^",   myfile);
	myfile = str_replace("=", "^-EQUAL-^",   myfile);

	if(append != "")
	  {
	  # append .html
	  s = strlen(append);
	  last.s = .substr(str, -1*s);
	  if(last.s != append)
	    {
	    myfile = paste0(myfile, append);
	    }
	  }
	myfile;
	}


#' cleanup.url
#'
#' @param url URL to be cleansed
#'
#' @return cleansed URL
#' @export
#'
#' @examples
#' cleanup.url("https://www.myprofiletraits.com//");
#' cleanup.url("https:/www.mshaffer.com//arizona//");
#'
cleanup.url = function(url)
	{
	url = str_replace("///", "/",   url);
	url = str_replace("//", "/",   url);
	url = str_replace(":/", "://", url); # https://
	url;
	}


dcf.get(key = "Version", pkg="stats")
	{
	key = tolower(key); # maybe do a pmatch?
	
	
	}

# i = help.parseFromLibrary(); str(i); dcf = .%$$%("i@dcf"); str(dcf);


dcf.uniqueKeys = function(pkgs = (.packages(all.available=TRUE)) )
	{
	h = help.parseFromLibrary(); str(h); dcf = .%$$%("h@dcf"); str(dcf);
	res = list.create(dcf$keys, dcf$values);
	res2 = list.create(dcf$keys, rep("base", length(dcf$keys)));
cat("\n", " INIT ... ", length(res), "\n"); Sys.sleep(0.25);
	# pkgs = (.packages(all.available=TRUE));
	n = length(pkgs);
	for(i in 1:n)
		{
		pkg = pkgs[i];
		h = help.get(pkg); dcf = dcf.parse( h$info[[1]] );
		res = list.update(res, dcf$keys, dcf$values);
		res2 = list.update(res2, dcf$keys, rep(pkg, length(dcf$keys)));
cat("\n", "\t ", i, " of ", n, "\t package:", pkg," ... ", length(res), "\n");
flush.console();
		}
	list("values" = res, "pkg" = res2);	
	}
	
# all.keys = dcf.uniqueKeys();  # ... all.keys = 81 with first val 
# str(all.keys);
# df = as.data.frame( cbind( names(all.keys$values), all.keys$pkg, all.keys$values ) ); rownames(df) = NULL; colnames(df) = c("keys", "package", "values"); head(df);



dcf.uniqueVals = function(key="Built", 
							pkgs = (.packages(all.available=TRUE)) 
						)
	{
	res = list();
	h = help.parseFromLibrary(); str(h); dcf = .%$$%("h@dcf"); str(dcf);
	s = v.find(dcf$keys, key);
	if(!is.null(s)) { res = list.update(res, "base", dcf$values[s]); }
	n = length(pkgs);
	for(i in 1:n)
		{
		pkg = pkgs[i];
		h = help.get(pkg); dcf = dcf.parse( h$info[[1]] );
		s = v.find(dcf$keys, key);
		if(!is.null(s)) { res = list.update(res, pkg, dcf$values[s]); }
cat("\n", "\t ", i, " of ", n, "\t package:", pkg," ... ", length(res), "\n");
flush.console();
		}
	
	#df = as.data.frame( cbind( names(res), unname(unlist(res)) ) );
	df = as.data.frame( cbind( names(res), as.character(res) ) );
		rownames(df) = NULL; colnames(df) = c("pkg", "val");
	df = df.sortBy(df, "pkg", "ASC"); head(df);
	df;	
	}
	
# all.vals = dcf.uniqueVals("Author"); head(all.vals); View(all.vals);
# idx = which.max(str.len(all.vals$val)); val = all.vals$val[idx]; all.vals$pkg[idx]; val; 
# idx = which(all.vals$pkg=="readr"); val = all.vals$val[idx]; all.vals$pkg[idx]; val;
# idx = rand(1, length(all.vals$pkg)); val = all.vals$val[idx]; all.vals$pkg[idx]; val;
	
# first RUN was "tibble" 
# pkgs = .packages(); np = length(pkgs); idx = sample(1:np, 1); pkg = pkgs[idx];  h = help.get(pkg); dcf = dcf.parse( h$info[[1]] ); str(dcf); print(pkg);
	
dcf.parse = function(dcfstr)
	{
	# h = help.get(pkg); dcf = dcf.parse( h$info[[1]] );
	# h = help.get(pkg); dcfstr = ( h$info[[1]] );
	# lined string, easily available within R scope ...
	# see help.parseFromLibrary
	info = str.explode(":", dcfstr);
	keys = list.getElements(info, 1);
	unkeyed = str.trim( str.replace( paste0(keys,":"), "", dcfstr) ); 
	## unkeyed are RAW, unparsed values 
	
	out = NULL;
	n = length(keys);
	for(i in 1:n)
		{
		key = keys[i];
		val = unkeyed[i];
		.key = tolower(key);
		if(.key == "built" || .key == "date" || .key == "date/publication" || .key == "repository/r-forge/datetimestamp" || .key == "packaged")
			{
			v = dcf.parseBuild(val);				
			out[[key]] = v; 
			next;
			}
		
		if(.key == "suggests" || .key == "depends" || .key == "imports")
			{
			pkgs = dcf.parseDepends(val);
			out[[key]] = pkgs;
			next;
			}
		
		if(.key == "url" || .key == "bugreports" || .key == "urlnote"|| .key == "additional_repositories" || .key == "urlnote")
			{
			ukey = dcf.parseURL(val);				
			out[[key]] = ukey;
			next;
			}
		
		if(.key == "author" || .key == "maintainer" || .key == "contact"|| .key == "authors@r")
			{
			people = dcf.parsePeople(val);			
			out[[key]] = people;
			next;
			}
		
		# DEFAULT
		out[[key]] = str.removeWhiteSpace(val);
		}
	
	## find URLs, find EMAILs, <email> prefered 
	## find [aut, cre, cph]
	## maybe it has @Rperson so must be evaluated ...
	##  r_code <- gsub("^`(.*)`$", "\\1", value)
   ##     if (nchar(r_code) != nchar(value)) {
    ##        settings[[s]] <- eval(parse(text = r_code))
     ##   }

	## BUILT is ";" sep 
	## PACKAGED is ";" TIME/WHO (r-profile?)
	## 
	
	
	
	
	out;
	# df = as.data.frame( cbind(keys, unkeyed) );
		# colnames(df) = c("keys", "values");
	# df;
	}



# idx = which(all.vals$pkg=="readr"); val = all.vals$val[idx]; all.vals$pkg[idx]; val;
# idx = rand(1, length(all.vals$pkg)); val = all.vals$val[idx]; all.vals$pkg[idx]; val;
# people = dcf.parsePeople(val); str(people);
 
## idx = rand(1, length(all.vals$pkg)); val = all.vals$val[idx]; all.vals$pkg[idx]; val; people = dcf.parsePeople(val); str(people); all.vals$pkg[idx];
## rgl is quite long, DIRK
## BH (also DIRK) ... with "and"  ... [ , and ]
## "Dirk Eddelbuettel, Romain Francois, Doug Bates, Binxiang Ni, and Conrad Sanderson"

## Atsushi Yasumoto in "rmarkdown"
# "Atsushi" "Yasumoto"  "[ctb,"  "cph]"  "(<https://orcid.org/0000-0002-8335-495X>," "Number"  "sections" "Lua" "filter),"


dcf.parsePeople = function(val)
	{
debug = FALSE;
	# this is the *HARD* one to do well...
	# let's just read, right=to=left 
	tmp = str.replace("\n", " ", val);
	tmp = str.removeWhiteSpace(tmp);
	## uuid 
	words = str.explode(" ",tmp);
if(debug)
	{
	print(words);
	}
	nwords = length(words);
	j = 1;
	newp = TRUE;
	stack = list();
	people = NULL;
	pidx = 1;
	while(j <= nwords)
		{
		word = words[j]; print(word);
		
		if(newp)
			{
if(debug)
	{
cat("\n --NEW P-- \n");
	}
			stack = list();
			what = "pname";
			if(word != "and")
				{
				stack[[what]] = word;	
				}
			newp = FALSE;
			j = 1 + j; 
			next;
			}
			
		{
		has.tag = str.contains("<", word);	
		has.bra = str.contains("[", word);
		has.braE = str.contains("]", word);
		has.par = str.contains("(", word);
		has.parE = str.contains(")", word);
		has.com = str.contains(",", word);
		has.and = (word == "and") ;	
		
		is.end = (has.and || has.com);
		end.r = c("and", ",")

		all = c(has.tag, has.bra, has.braE, has.par, has.parE, has.com, has.and);
if(debug)
	{
print(all);
	}	
		}
		if(sum(all) == 0) 
			{ 
if(debug)
	{
cat("\n --ALL-- \n");
	}
			stack[[what]] = str.trim(paste0(stack[[what]], " ", word));
			j = 1 + j; 
			next;
			} 
		
		
		if(has.bra || what == "role")
			{
cat("\n --BRACKET-- \n");
			what = "role";
			r = str.replace("[", "", word);
			if(has.braE)
				{
				r = str.explode("]",r)[1];
				stack[[what]] = c(stack[[what]] , r);
				what = "";
				if(is.end) 
					{
					people[[pidx]] = stack;
					pidx = 1 + pidx;
					newp = TRUE;
					j = 1 + j;
					next;
					}
				} else {
						r = str.replace("," , "", r);
						stack[[what]] = c(stack[[what]] , r);
						}
			j = 1 + j;
			next;
			}
		
		
		if(has.tag)
			{
if(debug)
	{
cat("\n -- <tag> -- \n");
	}
			e = str.between(word, keys=c("<",">"));
			# shouldn't be NA 
			if(what == "pname")
				{
				what = "email";				
				} else {
						what = "url";
						}
# "Atsushi" "Yasumoto"  "[ctb,"  "cph]"  "(<https://orcid.org/0000-0002-8335-495X>," "Number"  "sections" "Lua" "filter),"
			stack[[what]] = e;
			if(has.par) { what = "more"; } # for next iteration
			if(is.end && has.parE) 
				{
				people[[pidx]] = stack;
				pidx = 1 + pidx;
				newp = TRUE;			
				}
			j = 1 + j;
			next;
			}
		

		if(has.par || what == "more")
			{
if(debug)
	{
cat("\n -- (PARA) -- \n");
	}
			what = "more";
			m = str.trim( str.replace("(","",word) );			
			# (par ONE WORD parE)
			if(has.parE)
				{
				m = str.explode(")",m);	
				m = m[1];
				stack[[what]] = str.trim( paste0(stack[[what]], " ", m) );									
				what = "";
				if(has.com)  # not and, maybe a word in "more"
					{
					people[[pidx]] = stack;
					pidx = 1 + pidx;
					newp = TRUE;
					}	
				} else {
						stack[[what]] = str.trim( paste0(stack[[what]], " ", m) );
						}			
			j = 1 + j;
			next;
			}
		

		
		
		
		
		
		
		if(what == "pname")
			{
if(debug)
	{
cat("\n --PNAME-- \n");
	}
			# a person has "and" in their NAME ==> GONER ???
			m = word;
			if(is.end)
				{
				m = str.replace("," , "", m);
				stack[[what]] = str.trim(paste0( stack[[what]], " ", m));
				people[[pidx]] = stack;
				pidx = 1 + pidx;
				newp = TRUE;
				j = 1 + j;
				next;
				} else { 
						stack[[what]] = str.trim(paste0( stack[[what]], " ", m));						
						}
			# stack[[what]] = str.trim( str.replace("and ", "", stack[[what]] ) );
			j = 1 + j;
			next;
			}
		
			
		# if(is.end)
			# {
			# people[[pidx]] = stack;
			# pidx = 1 + pidx;
			# newp = TRUE;
			# j = 1 + j;
			# next;
			# }
		
if(debug)
	{
cat("\n END OF THE STACK, HOW??? \n");
	}
		# print(stack);
		# stack[[what]] = paste0( stack[[what]], " ", word);
		# j = 1 + j;
		}
	## last one ... outside of loop ... words ended 
	if(length(stack) > 0) 
		{ 
		people[[pidx]] = stack; 
		}
	
if(debug)
	{
print(words);
	}
	
	return(people);
	}
 

dcf.parseURL = function(val)
	{
debug = FALSE;
	# anything that looks like only URLS , 
	# idx = rand(1, length(all.vals$pkg)); val = all.vals$val[idx]; all.vals$pkg[idx]; val;
	# "dendextend"
	# "multcomp"
if(debug) {
			print(val); 
			dput(val);
			stop("monte");
			}
	tmp = str.removeWhiteSpace(str.explode(",", val));
	# "antiword" ... missing a comma
	tmp2 = unlist(str.explode(" ", tmp));
	u = check.url(tmp2);
	
	s = v.find(tmp2, u );
	ns = v.find(tmp2, !u );
	if(!is.null(s)) 
		{ 
		ukey = tmp2[s];
		if(!is.null(ns)) 
			{
			uval = tmp2[ns];
			ukey = property.set("more", ukey, uval);
			# names(ukey) = uval;
			}
		}			
	ukey;
	}



dcf.parseDepends = function(val)
	{
	# pkgs with (>version)
	tmp = str.removeWhiteSpace(str.explode(",", val));
	tmp2 = str.explode("(", tmp);
	pkgs = list.getElements(tmp2, 1);
		depe = list.getElements(tmp2, 2);
		depe[!is.na(depe)] = paste0("(", depe[!is.na(depe)]);
		depe[ is.na(depe)] = "";
	pkgs = property.set("dependencies", pkgs, depe);
	pkgs;
	}


dcf.parseBuild = function(val)
	{
	# has dates ... 
	v = list();					
	tmp = str.removeWhiteSpace(str.explode(";", val));
	s = v.find(tmp, str.contains("R", tmp) )[1];
	if(!is.null(s)) 
		{ 
		v[["R.raw"]] = tmp[s]; 
		v[["R.version"]] = str.trim(str.replace("R","",tmp[s]));
		}
	s = v.find(tmp, check.date(tmp) )[1];
	if(!is.null(s)) 
		{ 
		v[["date"]] = tmp[s];
		}			
	s = v.find(tmp, str.contains("win", tmp))[1];
	if(!is.null(s)) 
		{ 
		v[["win"]] = tmp[s];
		}
	s = v.find(tmp, str.contains("ming", tmp))[1];
	if(!is.null(s)) 
		{ 
		v[["ming"]] = tmp[s];
		}
		
	s = v.find(tmp, str.contains("tap", tmp))[1];
	if(!is.null(s)) 
		{ 
		v[["tap"]] = tmp[s];
		}
	v;
	}













































































































# parseTable = function(str)
# 	{
# 	# TODO
#
# 	}




#' parse.sliceDiceContent
#'
#' This takes a string and returns a substring based on some
#'  'cut points' defined as start and end.
#'
#' The main engine is strsplit so there has to be a direction.
#'
#' @param str
#' @param start
#' @param end
#' @param strip
#' @param direction
#'
#' @return
#' @export
#' 
#' @aliases parse.sliceDiceContent
parse.sliceDiceContent = function (str, start="<h2>",end="</h2>", strip=FALSE, direction="start")
		{
    # slice and dice ... could I build this as "multivariate"?
		d = substr(tolower(trimMe(direction)), 1,1);

		if(d == "e")  # starting at the end on the first cut ...
			{
			tmp = explodeMe(end,str);
				tmp1 = explodeMe(start,tmp[1]);
				if(length(tmp1) < 2 ) { tmp1[2] = ""; }
			str = tmp1[2];
			} else { # start .... beginning, whatever
					tmp = explodeMe(start,str);
					if(length(tmp) < 2 ) { tmp[2] = ""; }
						tmp1 = explodeMe(end,tmp[2]);
					str = tmp1[1];
					}

			if(strip) { str = trimMe(strip_tags(str)); }
		str;
		}



#' strip_tags
#'
#' Remove HTML tags from a string.
#'
#' @param htmlString
#'
#' @return
#' @export
#'
#' @aliases strip.tags striptags
strip_tags = function(htmlString)
	{
	return(gsub("<.*?>", "", htmlString));
	}








#' folderizeURL
#'
#'
#' Convert a url into a reversible folder structure.
#'
#' @param url
#'
#' @return
#' @export
#'
#' @examples
folderizeURL = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana")
  {
	# could be multivariate?
  # TODO: update to explodeMe and str_replace
  info = strsplit(url, "//", fixed=TRUE);
	str = info[[1]][2];
	prot = str.replace(":","", info[[1]][1]);

    find = c("/",".",",");
    replace = c("-","^","+");
    n.find = length(find);
  for(i in 1:n.find)
    {
    str = gsub(find[i],replace[i],str,fixed=TRUE);
    }
	str = paste0("__",prot,"__", str);
  str;
  }




#' stringifyLibrary
#'
#' If multiple users share a notebook (Dropbox), you can identify
#'  who they are by the R_LIBS_HOME or some other element
#'
#' @param str
#'
#' @return
#' @export
#'
#' @examples
stringifyLibrary = function(str)
  {
  # for shared notebooks
  str = gsub(" ","",str, fixed=TRUE);
  str = gsub("/","",str, fixed=TRUE);
  str = gsub(":","",str, fixed=TRUE);
  str = gsub("-","",str, fixed=TRUE);
  str = gsub(".","",str, fixed=TRUE);
  str;
  }


#' listToString
#'
#' Often paramaters for options can be collapsed to form a folder name or a md5 hash
#'
#'
#' @param mylist list to stringify, NOT RECURSIVE (one level)
#' @param sep1 "-"
#' @param sep2 "_"
#'
#' @return string
#' @export
#'
#' @examples
#'
#' my.list = list("A" = 3, "L" = ".", "E" = 1, "X" = 4);
#'     listToString(my.list);
#'
listToString = function(mylist,sep1="-",sep2="_")
  {
  str = c();
  mynames = names(mylist);
  for(myname in mynames)
    {
    val = mylist[[myname]];
    str = c(str, paste0(myname,sep1,val));
    }
  paste0(str,collapse=sep2);
  }



























































#' getKeysFromStringWithSeparator
#'
#' @param str What is the string?
#' @param sep What separates elements in string?
#' @param lower.case Convert input to lowercase?
#'
#' @return a character vector
#' @export
#'
#' @examples
#' str = "Actor,Producer,Writer";
#' keys = getKeysFromStringWithSeparator(str);
#' keys;
getKeysFromStringWithSeparator = function(str, sep=",", lower.case=TRUE)
      {
      if(lower.case) { str = tolower(str);}
      vals = explodeMe(sep, str);
      f.vals = c();
      for(val in vals)
        {
        val = trimMe(val);
        f.vals = c(f.vals,val);
        }
      f.vals;
      }



# normalizePath("C:/Users/Alexander Nevsky/Dropbox/WSU-419/Fall 2020/__student_access__/sample_latex_files/Multivariate-2009/datasets/MBA_CAR_ATTRIB.txt");
# windowsNormalize ...









