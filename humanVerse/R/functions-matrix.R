

#' matrixTrace
#'
#' @param square a square matrix (n x n)
#'
#' @return numeric value for trace(matrix)
#' @export
#' @aliases traceMatrix
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' matrixTrace(m);
#'
matrixTrace = function(square)
  {
  sum(diag(square));
  }


#' transposeMatrix
#'
#' @param mat a matrix
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' transposeMatrix(m);
#'
transposeMatrix = function(mat)
	{
	t(mat);
	}

#' rotateMatrix
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix(m);
#' rotateMatrix(m,FALSE);
#'
rotateMatrix = function(x,clockwise=TRUE)
	{
	if(clockwise)
		{
		t( apply(x, 2, rev) );
		} else 	{
				apply( t(x),2, rev);
				}
	}

#' rotateMatrix90
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix90(m);
#' rotateMatrix90(m,FALSE);
#'
rotateMatrix90 = function(x,clockwise=TRUE)
	{
  rotateMatrix(x,clockwise);
	}

#' rotateMatrix180
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix180(m);
#' rotateMatrix180(m,FALSE);  # equivalent
#'
rotateMatrix180 = function(x,clockwise=TRUE)
	{
  rotateMatrix(
    rotateMatrix(x,
			clockwise),
		clockwise);

	}

#' rotateMatrix270
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix270(m);
#'
#' rotateMatrix270(m,FALSE);
#' rotateMatrix90(m); # equivalent
#'
rotateMatrix270 = function(x,clockwise=TRUE)
	{
  rotateMatrix(
    rotateMatrix(
      rotateMatrix(x,
			clockwise),
		clockwise),
	clockwise);
	}


#' rotateMatrixAngle
#'
#' @param x matrix
#' @param a angle, must be a multiple of 90 degrees, can be 630
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#'
#' rotateMatrixAngle(m,0);
#' rotateMatrixAngle(m,90);
#' rotateMatrixAngle(m,180);
#' rotateMatrixAngle(m,270);
#' rotateMatrixAngle(m,-90); # we reverse the direction of clockwise if negative
#'
#' rotateMatrixAngle(m,270,FALSE);
#' rotateMatrixAngle(m,-90,FALSE); # we reverse the direction of clockwise if negative
#'
#' #rotateMatrixAngle(m,33);
#' rotateMatrixAngle(m,630);
#'
rotateMatrixAngle = function(x, a=0, clockwise=TRUE)
	{
	rem = a %% 90;
	if(rem != 0)
		{
		stop("Not a valid angle");
		}

	if(a < 0)
	  {
	  # let's reverse the direction
	  clockwise = if(clockwise) FALSE else TRUE;
	  a = abs(a);
	  }
	div = a %% 360;

	switch( as.character(div),
		"90" 	= rotateMatrix90 (x,clockwise),
		"180" = rotateMatrix180(x,clockwise),
		"270" = rotateMatrix270(x,clockwise),
		x
		);

	}




matrix.setNumbersToMatrixNames = function (ss.idx, A, pre="P.", post="")
  {
  # row/col names store the adjacency indexing as "(P)oint.NNN"
  rnames = paste0(pre, ss.idx, post);
  rownames(A) = colnames(A) = rnames;
  A;
  }

matrix.getNumbersFromMatrixNames = function (rnames, pre="P.", post="")
  {
    if(pre  != "") { rnames = str_replace(pre, "", rnames);  }
    if(post != "") { rnames = str_replace(post, "", rnames); }
  as.numeric( rnames );
  }

matrix.sortAdjacency = function(A, decreasing=TRUE)
  {

  }


matrix.rowNormalize = function(A)
  {

  }

# what = "add", or "remove" ... (a)ppend or (r)emove ...
matrix.superNode = function(A, what = "add", where = "b,r")  # t/b, l/r
  {
  what = substr( trimMe(tolower(what)), 1, 1);
  if(what != "a") { what = "r"; } # anything that doesn't start with a

  tmp = explodeMe(",", trimMe(tolower(where)) );
    v = substr(tmp[1], 1, 1); if(v != "b") { v = "t"; } # (t)op
    h = substr(tmp[2], 1, 1); if(h != "r") { h = "l"; } # (l)eft




  }


