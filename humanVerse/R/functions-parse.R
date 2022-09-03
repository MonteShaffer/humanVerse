


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
	
# all.vals = dcf.uniqueVals("Built"); head(all.vals); View(all.vals);
# idx = which.max(str.len(all.vals$val)); val = all.vals$val[idx];	
	
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
		if(.key == "built")
			{
			v = list();					
			tmp = str.removeWhiteSpace(str.explode(";", val));
			r = str.contains("R", tmp); s = v.find(tmp, r)[1];
			if(!is.null(s)) 
				{ 
				v[["R.raw"]] = tmp[s]; 
				v[["R.version"]] = str.trim(str.replace("R","",tmp[s]));
				}
			d = check.date(tmp); s = v.find(tmp, d)[1];
			if(!is.null(s)) 
				{ 
				v[["date"]] = tmp[s];
				}			
			w = str.contains("win", tmp); s = v.find(tmp, d)[1];
			if(!is.null(s)) 
				{ 
				v[["win"]] = tmp[s];
				}
			m = str.contains("ming", tmp);  s = v.find(tmp, d)[1];
			if(!is.null(s)) 
				{ 
				v[["ming"]] = tmp[s];
				}
			out[[key]] = v; 
			next;
			}
		
		if(.key == "suggests")
			{
			tmp = str.removeWhiteSpace(str.explode(",", val));
			tmp2 = str.explode("(", tmp);
			pkgs = list.getElements(tmp2, 1);
				depe = list.getElements(tmp2, 2);
				depe[!is.na(depe)] = paste0("(", depe[!is.na(depe)]);
				depe[ is.na(depe)] = "";
			pkgs = property.set("dependencies", pkgs, depe);
			
			out[[key]] = pkgs;
			next;
			}
		
		# DEFAULT
		out[[key]] = val;
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
	
	
	
	
	
	df = as.data.frame( cbind(keys, unkeyed) );
		colnames(df) = c("keys", "values");
	df;
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









