# slice and dice
sliceDiceContent = function (str, start="<h2>",end="</h2>", strip=FALSE, direction="end")
		{
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



strip_tags <- function(htmlString) 
	{
	return(gsub("<.*?>", "", htmlString))
	}








folderizeURL = function(url = "https://en.wikipedia.org/wiki/Columbia_Falls,_Montana")
  {
  str = strsplit(url, "//", fixed=TRUE)[[1]][2];
    find = c("/",".",",");
    replace = c("-","^","+");
    n.find = length(find);
  for(i in 1:n.find)
    {
    str = gsub(find[i],replace[i],str,fixed=TRUE);
    }
  str;
  }



# for shared notebooks
stringifyLibrary = function(str)
  {
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
      # I believe there are two functions strsplit and str_split.  I should pick on.
      if(lower.case) { str = tolower(str);}
      vals = stringr::str_split(str,sep);
      f.vals = c();
      for(val in vals)
        {
        val = stringr::str_trim(val);
        f.vals = c(f.vals,val);
        }
      f.vals;
      }



# normalizePath("C:/Users/Alexander Nevsky/Dropbox/WSU-419/Fall 2020/__student_access__/sample_latex_files/Multivariate-2009/datasets/MBA_CAR_ATTRIB.txt");
# windowsNormalize ...









