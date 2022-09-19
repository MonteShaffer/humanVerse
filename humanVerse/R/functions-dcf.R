
# str(dcf.get("tibble"));
# str(dcf.get(tibble)); 
dcf.get = function(..., return="list", character.only = FALSE)
	{ 
	RETURN = prep.arg(return, 1);
debug = FALSE;
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="stringi");
	if(!character.only) { pkgs = as.character(pkgs); }

	# if not pkg true name, character.only
	idx = check.pkgs(pkgs, character.only = TRUE);
		if(is.null(idx)) { cat.stop("no good packages"); }
	good = pkgs[idx];
dput(good);

	n = length(good);
	badns = NULL;
	res = list();
	for(i in 1:n)
		{
		pkg = good[i];
		pkg.ns = suppressError( getNamespace(pkg), show.notice=debug, msg="debug dcf.get ");
		if(is.error(pkg.ns)) { badns = c(badns, pkg); next; }		
		# CACHING mechanism as JSON files
	
		# get the data 
		h = help.get(pkg); dcf = dcf.parse( h$info[[1]] );
		if(RETURN == "j") { json = JSON.stringify(dcf); return(dcf); }
		dcf;
		}
		
	list.return(res);
	}


check.ns = function(..., character.only=FALSE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="stringi");
	if(!character.only) { pkgs = as.character(pkgs); }
debug = FALSE;
	n = length(pkgs);
	idx = logical(n); # FALSE by default 
	for(i in 1:n)
		{
		pkg = pkgs[i];
		pkg.ns = suppressError( getNamespace(pkg), 
									show.notice=debug, 
									msg="debug dcf.get "
							);
		if(!is.error(pkg.ns)) { idx[i] = TRUE; }	 
		}
	bad = v.return(pkgs[!idx]);
		if(!is.null(bad)) { cat.warning("\n\n", "bad package names spaces: maybe not installed", bad, "\n\n"); }
	idx;	
	}


# this works 
# environmentName(getNamespace("stringi"))



check.pkg = function(..., character.only=FALSE, name.space=TRUE)
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="stringi");
	if(!character.only) { pkgs = as.character(pkgs); }
	
	# if not pkg true name, character.only
	idx = fidx = is.library(pkgs, character.only = TRUE);
		# is.library alreayd has a warning ... 
	bad = v.return(pkgs[!idx]);
		if(!is.null(bad)) { cat.warning("\n\n", "bad package names: maybe not installed", bad, "\n\n"); }
		
	if(name.space)
		{
		idx2 = check.ns(pkgs, character.only = TRUE);
		fidx = (idx & idx2);
		}	
		
	v.return(fidx);
	}

# dcf.getKey(tibble, "Version");
# dcf.getKey("tibble", "Version");
dcf.getKey = function(..., key = "Version")
	{
	pkgs = prep.dots(..., collapse=character.only, has.objects=!character.only, default="stringi");
	if(!character.only) { pkgs = as.character(pkgs); }
	
# TODO, update this ... allow multiple keys ... dataframe regarless
# rows are pkgs, cols are keys ... 
# easier way to get DESCRIPTION file 
# maybe a wrapper function with a CHOICE ... 
# help.get ... go to the folder, go to the web ... SO had another idea, I forget


stop("monte: update this");
	pkg = str.fromObjectName(...);
	# key = tolower(key); # maybe do a pmatch?
	# h = help.get(pkg); dcf = dcf.parse( h$info[[1]] ); 
	dcf = dcf.get(pkg);  # has caching 	
	dcf[[key]]; # AUTOMATICALLY returns NULL if not found 	
	}





# first RUN was "tibble"  
# pkgs = .packages(); np = length(pkgs); idx = sample(1:np, 1); pkg = pkgs[idx];  h = help.get(pkg); dcf = dcf.parse( h$info[[1]] ); str(dcf); print(pkg);
	
dcf.parse = function(dcfstr)
	{
debug = FALSE;
	# h = help.get(pkg); dcf = dcf.parse( h$info[[1]] );
	# h = help.get(pkg); dcfstr = ( h$info[[1]] );
	# lined string, easily available within R scope ...
	# see help.parseFromLibrary
	info = str.explode(":", dcfstr);
	keys = list.getElements(info, 1);
	unkeyed = str.trim( str.replace( paste0(keys,":"), "", dcfstr) ); 
	## unkeyed are RAW, unparsed values 

if(debug) 
	{
print(keys);
	}
	
	n = length(keys);
	# out = vector("list", n);	
	out = NULL; # "tibble" has weirdness at top 
	for(i in 1:n)
		{
		key = keys[i];
		val = unkeyed[i];
		.key = tolower(key);
		if(.key == "") { next; }
		
		
		if(.key == "built" || .key == "date" || .key == "date/publication" || .key == "repository/r-forge/datetimestamp" || .key == "packaged" || .key == "license")
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
			
			if(.key == "authors@r")
				{
				# needs ", " ... the space is important for *words* parser
				val = paste0( eval(parse(text = val)), collapse=", ");
				}
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
		word = words[j]; # print(word);
		
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
if(debug)
	{
cat("\n --BRACKET-- \n");
	}
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
if(debug) 
	{
print(val); 
dput(val);
stop("monte");
	}
	tmp = str.removeWhiteSpace(str.explode(",", val));
	# "antiword" ... missing a comma
	tmp2 = unlist(str.explode(" ", tmp));
	u = check.url(tmp2);
	
	ukey = NULL; 
	s = v.which(tmp2, u );
	ns = v.which(tmp2, !u );
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
debug = FALSE;
	# pkgs with (>version)
	tmp = str.removeWhiteSpace(str.explode(",", val));
	tmp2 = str.explode("(", tmp);
if(debug) 
	{
print(tmp);
print(tmp2);
	}
	pkgs = list.getElements(tmp2, 1);
		depe = list.getElements(tmp2, 2);
		depe[!is.na(depe)] = paste0("(", depe[!is.na(depe)]);
		depe[ is.na(depe)] = "";
	pkgs = property.set("dependencies", pkgs, depe);
	pkgs;
	}







dcf.parseBuild = function(val)
	{
debug = FALSE;
	# has dates ... 
	v = list();					
	tmp = str.removeWhiteSpace(str.explode(";", val));
if(debug) 
	{
print(tmp);
	}
	s = v.which(tmp, str.contains("R", tmp) )[1];
	if(!is.null(s)) 
		{ 
		v[["R.raw"]] = tmp[s]; 
		v[["R.version"]] = str.trim(str.replace("R","",tmp[s]));
		tmp = v.truncate(tmp[s], tmp);
		if(is.null(tmp)) { return(v); }
		}
	s = v.which(tmp, check.date(tmp) )[1];
	if(!is.null(s)) 
		{ 
		v[["date"]] = tmp[s];
		tmp = v.truncate(tmp[s], tmp);
		if(is.null(tmp)) { return(v); }
		}			
	s = v.which(tmp, str.contains("win", tmp))[1];
	if(!is.null(s)) 
		{ 
		v[["win"]] = tmp[s];
		tmp = v.truncate(tmp[s], tmp);
		if(is.null(tmp)) { return(v); }
		}
	s = v.which(tmp, str.contains("ming", tmp))[1];
	if(!is.null(s)) 
		{ 
		v[["ming"]] = tmp[s];
		tmp = v.truncate(tmp[s], tmp);
		if(is.null(tmp)) { return(v); }
		}
		
	s = v.which(tmp, str.contains("tap", tmp))[1];
	if(!is.null(s)) 
		{ 
		v[["tap"]] = tmp[s];
		tmp = v.truncate(tmp[s], tmp);
		if(is.null(tmp)) { return(v); }
		}
		v[["more"]] = tmp; 
	v;
	}





















###################### USEFUL FOR EDGE CASES
### DEBUGGER functions ... get uniqueKeys from PKGS
###                    ... get uniqueVals for KEY from PKGS 

# i = help.parseFromLibrary(); str(i); dcf = .%$$%("i@dcf"); str(dcf);

dcf.uniqueKeys = function() {}
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










dcf.uniqueVals = function() {}
dcf.uniqueVals = function(key="Built", 
							pkgs = (.packages(all.available=TRUE)) 
						)
	{
	res = list();
	h = help.parseFromLibrary(); str(h); dcf = .%$$%("h@dcf"); str(dcf);
	s = v.which(dcf$keys, key);
	if(!is.null(s)) { res = list.update(res, "base", dcf$values[s]); }
	n = length(pkgs);
	for(i in 1:n)
		{
		pkg = pkgs[i];
		h = help.get(pkg); dcf = dcf.parse( h$info[[1]] );
		s = v.which(dcf$keys, key);
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
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	