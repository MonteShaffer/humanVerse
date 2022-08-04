
##################################################
#'
#' is.library
#'
#'
#' @param str (what is the character string to be searched)
#'
#' @return TRUE or FALSE
#' @export
#'
#' @examples
is.library = function(str = "stringi")
	{
	isTRUE(requireNamespace( str , quietly = TRUE))
	}


