


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









