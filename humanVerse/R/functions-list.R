








##################################################
#'
#' list.return
#'
#' If list only has one element return it.
#' By default unlist so it is just a vector ... (redundant with above?)
#'
#' @param res
#' @param unlist
#'
#' @return
#' @export
list.return = function(res, unlist=FALSE)
	{
	nr = length(res);
		if(nr == 0) { return (NULL); }
		if(nr == 1) { return (res[[1]]); }
	if(unlist) { unlist(res); } else { res; }
	}


#' @rdname returnList
#' @export
returnList = list.return;



##################################################
#'
#' list.extract
#'
#' This is not recurive, gets "names" of lists and extracts them to the environment as accessible variables using assign.
#'
#' @param myList the list to be extracted
#' @param envir the environment inwhich to extract it
#'
#' @return updates and assigns the values
#' @export
#'
#' @examples
#'
#' mylist = list("a" = 1, "b" = 2, "c" = 3);
#'         list.extract(mylist);
#'         print(a); print(b); print(c);
#'
#'
list.extract = function(myList, envir = .GlobalEnv)
    {
    n.myList = length(myList);
    if(n.myList > 0)
      {
      for(i in 1:n.myList)
        {
        assign(names(myList)[i], myList[[i]], envir = .GlobalEnv);
        }
      }
    }


#' @rdname extractList
#' @export
extractList = list.extract;


# https://stackoverflow.com/questions/44176908/
list.getElements = function(info, n=1)
	{
	n.info = length(info);
	if(!is.list(info)) { return(info[n]); }
	if(n.info == 0) { return(NULL); }
	## sapply(info, "[[", n);  # this doesn't work with MISSING/NULL
	## ... this would be nice ... info[[,2]] or info[[*2]]
	res = NULL;
	for(i in 1:n.info)
		{
		res[i] = info[[i]][n];  # will put NA if missing here
		}
	res;
	}




