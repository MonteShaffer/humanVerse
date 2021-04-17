


  

#' printPaste0
#'
#' merges print(paste0 into one function
#'
#' @param ...  one or more R objects, to be converted to character vectors.
#' @param collapse an optional character string to separate the results
#' @param recycle0 TRUE / FALSE
#'
#' @return prints the result
#' @export
printPaste0 = function(... , collapse = NULL, recycle0 = FALSE)
  {
  # paste0(..., collapse) is equivalent to paste(..., sep = "", collapse), slightly more efficiently.
  print(paste0(... , collapse = collapse, recycle0 = recycle0) );
  }



#' printPaste
#'
#' merges print(paste into one function
#'
#' @param ...  one or more R objects, to be converted to character vectors.
#' @param sep a character string to separate the terms
#' @param collapse an optional character string to separate the results
#' @param recycle0 TRUE / FALSE
#'
#' @return prints the result
#' @export
printPaste = function(... , sep = " ", collapse = NULL, recycle0 = FALSE)
  {
  print(paste(... , sep = sep, collapse = collapse, recycle0 = recycle0) );
  }



#' printMatrix
#'
#' @param mat numeric matrix
#' @param digits numeric, digits to round
#' @param zeroIsh if TRUE, zeroIsh is also called
#' @param z.digits numeric, digits to perform zeroIsh
#'
#' @return matrix, as character string
#' @export
#'
#' @examples
#' x = cbind( stats::rnorm(10,0,1), stats::rnorm(10,3,1), stats::rnorm(10,9,1) );
#' printMatrix( stats::cov(x), 3);
#' printMatrix( stats::cor(x), 3);
#'
#' x = matrix( c(sin(pi), cos(pi), -sin(pi), -cos(pi)), nrow=2);
#' zeroIsh(x);
#' printMatrix( x, 3 );
#' printMatrix( x, 22, zeroIsh = FALSE);
#' printMatrix( x, 22, zeroIsh = TRUE);
#' printMatrix( x, 22, zeroIsh = TRUE, z.digits=16);
printMatrix = function(mat, digits=3, zeroIsh=TRUE, z.digits=6) # align decimals ? ... center ... latex
  {
  n.rows = nrow(mat);
  my.row.names = rownames(mat);
  my.col.names = colnames(mat);

  mat.round = round(mat, digits = digits);
  if(z.digits > 22) { z.digits = 22; } # is this the maximum on all systems?
  if(zeroIsh) { mat.round = zeroIsh(mat.round, z.digits); }

  mat = as.character( mat.round );
  mat = matrix(mat, nrow=n.rows, byrow=FALSE);
    rownames(mat) = my.row.names;
    colnames(mat) = my.col.names;
  print(mat);
  }




#' roundMeToString
#'
#' @param val a numeric value
#' @param digits number of digits to round
#' @param leadingZero TRUE / FALSE ... include the leading zero, if necessary
#' @param decimal.sep The decimal separator, default is "."
#'
#' @return string of numeric based on parameters
#' @export
#'
#' @examples
#' roundMeToString(pi, 5);
#' roundMeToString(3.1415926535897932384626, 9);
#'
#' roundMeToString(0.9, 3);
#' roundMeToString(0.9, 3, FALSE);
#'
#' roundMeToString(1.9, 3);
#' roundMeToString(1.9, 3, FALSE);
#'
#' roundMeToString(-0.9, 3);
#' roundMeToString(-0.9, 3, FALSE);
roundMeToString = function(val, digits=3, leadingZero=TRUE, decimal.sep=".")
  {
  isNegative = (val < 0);
  isLessThanOne = (val > 0 && val < 1);

  str = as.character( round(val, digits=digits) );
  tmp = explodeMe(decimal.sep, str);

  whole = tmp[1];
  decimal = tmp[2];

  delta = digits - strlen(decimal);
  if(delta > 0)
    {
    decimal = strPadRight(decimal, digits, "0");
    }

  if(whole == "0" && !leadingZero) { whole = "";}

  paste0(whole, decimal.sep, decimal);
  }


truncateNumeric = function(val, digits=3, append="...")
	{
	strvec = as.character(val);
		tmp = explodeMe(".", strvec, NULL);
	whole 	= getElementsInList(tmp,1);
	decimal = getElementsInList(tmp,2);
		decimal = strPadRight(decimal, digits, "0");
	
	res = paste0(whole, ".", substr(decimal, 1, digits), append );
	
	res;
	}







