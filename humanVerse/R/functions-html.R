






#' castStringAsHTML
#'
#' RStudio/Knitr doesn't play nice with external HTML, this may fix it?
#'
#' @param str string that is HTML code
#'
#' @return same string with appended attribute
#' @export
castStringAsHTML = function(str)
	{
  # remote / local file?
	str = setAttribute("html", TRUE, str);

	class(str) = c("html", "character");
	str;
	}


