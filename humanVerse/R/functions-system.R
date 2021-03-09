

# No warning is generated if the variable does not exist. That means empty() is essentially the concise equivalent to !isset($var) || $var == false.
# https://www.php.net/manual/en/function.empty.php
is.empty = function(x)
  {
  # if(!obj.exists(x)) { return (TRUE); }
  # below is the function obj.exists, but it has to run at this level, not a passthru
  x.str = deparse(substitute(x));  
  if(!exists(x.str)) { return (TRUE); }
  
  
  
  if(is.null(x)) { return (TRUE); }
  if(length(x) == 0 ) { return (TRUE); }
  if(is.na(x)) { return (TRUE); }
  if(trimMe(x) == "") { return (TRUE); } # nothing inside (except maybe white space)

# can I be certain ???
  return (FALSE);
  }
  
# similar to `dir.exists` or `file.exists` 
obj.exists = function(x)
	{
	x.str = deparse(substitute(x));
	exists(x.str);
	}

# maybe use my `$$` accessor function for exists ...


# https://www.php.net/manual/en/function.isset.php
is.set = function(x)
  {

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


