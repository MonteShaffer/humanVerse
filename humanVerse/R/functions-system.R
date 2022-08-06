


#' returnList
#'
#' If list only has one element return it.
#' By default unlist so it is just a vector ... (redundant with above?)
#'
#' @param res
#' @param unlist
#'
#' @return
#' @export
returnList = function(res, unlist=TRUE)
	{
	nr = length(res);
		if(nr == 0) { return (NULL); }
		if(nr == 1) { return (res[[1]]); }
	if(unlist) { unlist(res); } else { res; }
	}




#' is.empty
#'
#' @param x
#'
#' @return
#' @export
is.empty = function(x)
  {

  # # source('C:/_git_/github/MonteShaffer/humanVerse/humanVerse/R/functions-file.R')

# No warning is generated if the variable does not exist. That means empty() is essentially the concise equivalent to !isset($var) || $var == false.
# https://www.php.net/manual/en/function.empty.php
## the variable has to be passed in isolation, so it is not "substitute/parsed"
## my.tmp = Sys.getenv("TMP"); is.empty(my.tmp); 	# works as expected
## is.empty(Sys.getenv("TMP"));						# does **NOT** work as expected


  # if(!obj.exists(x)) { return (TRUE); }
  # below is the function obj.exists, but it has to run at this level, not a passthru
  x.str = deparse(substitute(x));
  if(!exists(x.str)) { return (TRUE); }

  if(trimMe(x) == "") { return (TRUE); } # nothing inside (except maybe white space)




  if(is.null(x)) { return (TRUE); }
  if(length(x) == 0 ) { return (TRUE); }
  if(is.na(x)) { return (TRUE); }

# can I be certain ???
  return (FALSE);
  }


#' obj.exists
#'
#' @param x
#'
#' @return
#' @export
obj.exists = function(x)
	{
  # similar to `dir.exists` or `file.exists`
	x.str = deparse(substitute(x));
	exists(x.str);
	}


#' is.set
#'
#' @param x
#'
#' @return
#' @export
is.set = function(x)
  {
# maybe use my `$$` accessor function for exists ...


# https://www.php.net/manual/en/function.isset.php
  }


#' list.element.exists
#'
#' Traps error in tryCatch for non-existent list which `exists` rejects
#'
#' @param listvar Variable Name of list
#' @param element Element in list to check ... can be number or string
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
#'
#' list.element.exists(some.random.nonexistent.element, "base");
#'
#' my.list = list("a"=42, "b"=NULL);
#'   list.element.exists(my.list, "a");  # technically [["a"]] and [[1]] works in R
#'   list.element.exists(my.list, "b");  # null, but exists by name
#'   list.element.exists(my.list, "c");
#'   list.element.exists(my.list, 1);    # technically [["a"]] and [[1]] works in R
#'   list.element.exists(my.list, 2);   # it is technically NULL, but exists by name
#'   list.element.exists(my.list, 99);  # you shouldn't be mixing numeric / string ?
#'
#' my.list = list(); my.list[[1]] = "hi"; my.list[[3]] = "alex";
#'   list.element.exists(my.list, 1);
#'   list.element.exists(my.list, 2);  # technically R may set this equal to NULL
#'   list.element.exists(my.list, 3);
#'   list.element.exists(my.list, 99);
#'
list.element.exists = function(mylist, myelement)
  {
  # does list element exist?
  # could be a numeric or a key
  # https://stackoverflow.com/questions/7719741/how-to-test-if-list-element-exists
  tryCatch(
            {
            len = length(mylist); # error means doesn't exist
            if(len == 0) { return(FALSE); } # sub-element can't exist
            elen = length(mylist[[myelement]]);
            # print(elen);
            if(elen > 0) { return(TRUE); } # has data

            mynames = names(mylist);
            if(is.element(myelement,mynames)) { return(TRUE); }

            if(is.numeric(myelement))
              {
              myname = mynames[myelement];
              if(!is.na(myname))
                {
                if(is.element(myname,mynames)) { return(TRUE); }
                }
              }


            # is.null(listvar[[element]]);
            # could exist but be NULL

            # res = exists(element, where = listvar);
            # if(length(listvar[[element]]) > -1) { return(TRUE);}
            }, error = function(e)
                {
                return(FALSE);
                }
          );
  return(FALSE); # no errors?  ... may be NULL
  }



#' extractList
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
#'         extractList(mylist);
#'         print(a); print(b); print(c);
#'
#'
# list.extract 
extractList = function(myList, envir = .GlobalEnv)
    {
    n.myList = length(myList);  # maybe create an extract function ...
                                  # parent.env() ...
    if(n.myList > 0)
      {
      for(i in 1:n.myList)
        {
        assign(names(myList)[i], myList[[i]], envir = .GlobalEnv);
        }
      }
    }



extractList = function(myList, envir = parent.env(environment()) )
    {
	# .GlobalEnv
    n.myList = length(myList);  # maybe create an extract function ...
                                  # parent.env() ...
    if(n.myList > 0)
      {
      for(i in 1:n.myList)
        {
        assign(names(myList)[i], myList[[i]], envir = envir);
        }
      }
    }

# https://stackoverflow.com/questions/8771942/how-can-i-reference-the-local-environment-within-a-function-in-r
f <- function() {
  function() list(curEnv=environment(), parent=parent.env(environment()), 
          grandParent=parent.env(parent.env(environment())), callStack=sys.frames(), 
          callStackDepth=sys.nframe())
}
g <- function(f, n=2) if (n>2) g(f, n-1) else f()

floc <- f() # generate a local function
g(floc, 3) # call it


# "%in%" <- function(x, table) match(x, table, nomatch = 0) > 0
# is.element for an S-compatible equivalent of %in%.



