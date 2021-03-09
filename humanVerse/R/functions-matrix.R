


# matrixcalc is boring
# Matrix might have some cool stuff ... sparse matrix 


matrixRank = function(A, ...)
	{
	# maybe let them pass in a method?
	if (requireNamespace("Matrix", quietly = TRUE))
		{
		Matrix::rankMatrix(A, ...);	
		} else {
				if (requireNamespace("matrixcalc", quietly = TRUE))
					{
					matrixcalc::matrix.rank(A, ...);
					}
				}
	stop("I need a matrix library for this!");
	}

is.square.matrix = function(A)
	{
	if(!is.matrix(A)) { warning("Not a matrix"); return NA; } # FALSE ?
	nr = nrow(A);
	nc = ncol(A);
	return (nr == nc);
	}

# # should we consider "multiply.cpp" as option? ... larger matrices ... let's see in future ...
matrixPower = function(A, pow)
	{
	nr = nrow(A);
	pow = as.integer(pow);
	
	if(pow == 0) { return( diag(1, nr) ); }
	if(pow == 1) { return( A ); }
	if(pow < 0)
		{
		A.inv = solve(A);  # maybe use ginv?
		if(pow == -1) { return ( A.inv ); }
		A.copy = A.inv;
		for(i in 2:(-pow))
			{
			A.copy = A.copy %*% A.inv;
			}
		return ( A.copy );	
		} else {
				# positive	
				A.copy = A;
				for(i in 2:pow)
					{
					A.copy = A.copy %*% A;
					}
				return ( A.copy );	
				}
	}
	
	
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





# QUADRATIC FORM OF MATRIX ...
# http://math.ucdenver.edu/~esulliva/LinearAlgebra/SlideShows/07_02.pdf



# compute eigenRank here using the Romani / Del Corsi approach
# append super node to end, use inverse "solve" or ginv

# A = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0), .Dim = c(10L,  10L), .Dimnames = list(c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6",  "P.7", "P.8", "P.9", "P.10"), c("P.1", "P.2", "P.3", "P.4", "P.5",  "P.6", "P.7", "P.8", "P.9", "P.10")))

# A = structure(c(0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(10L,  10L), .Dimnames = list(c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6",  "P.7", "P.8", "P.9", "P.10"), c("P.1", "P.2", "P.3", "P.4", "P.5",  "P.6", "P.7", "P.8", "P.9", "P.10")))

matrix.rowNormalize = function(A)
	{
	A / rowSums(A);	
	}

matrix.convertMatrixToAdjacency = function(A, removeDiagonal=TRUE, scaleNegatives=TRUE, new.range = c(0,1))
	{
	# https://en.wikipedia.org/wiki/Perron%E2%80%93Frobenius_theorem#Positive_matrices
	# let's scale to all positive if we have negatives.  
	# Won't have "expected" Perron-Frobenius meaning otherwise
	if(scaleNegatives)
		{
		A = standardizeFromOldRangeToNew(A, newrange=new.range); # oldrange is figured out automatically
		}	
	if(removeDiagonal)
		{
		diag(A) = 0;  # this is a weird assignment approach ...	
		}
	A;
	}
	
	
matrix.removeSuperNode = function(A)
	{
	n = nrow(A) - 1;
	A[1:n,1:n];
	}
	
matrix.addSuperNode = function(A, name="=SUPER=")
	{
	n = nrow(A) + 1;
		A. = A;
		A. = rbind(A., 1);
		A. = cbind(A., 1);
		
		rownames(A.)[n] = colnames(A.)[n] = name;
	A.[n,n] = 0;
	A.;
	}
	
	


matrix.sortAdjacencyMatrix = function(A)
	{
	original = remaining = colnames(A);
	new = c();
	# A.copy = A; 
	
	cs = colSums(A);
	rs = rowSums(A);
	
	ts = rs+cs;
		# these are zero blocks ... "dangling"
		which.tz = which(ts == 0);  
		which.tz.names = names(which.tz);
		n = length(which.tz.names);
		if(n > 0)
			{
			for(i in 1:n)
				{
				if(!is.element(which.tz.names[i], new))  # function is vectored, but still
					{
					new = c(new, which.tz.names[i]);
					remaining = removeValueFromVector(which.tz.names[i], remaining);
					}
				}
			}
		
	
		# zero cols
		which.cz = which(cs == 0);
		which.cz.names = names(which.cz);
		n = length(which.cz.names);
		if(n > 0)
			{
			for(i in 1:n)
				{
				if(!is.element(which.cz.names[i], new))  # function is vectored, but still
					{
					new = c(new, which.cz.names[i]);
					remaining = removeValueFromVector(which.cz.names[i], remaining);
					}
				}
			}

	
		# zero rows
		which.rz = which(rs == 0);
		which.rz.names = names(which.rz);
		n = length(which.rz.names);
		if(n > 0)
			{
			for(i in 1:n)
				{
				if(!is.element(which.rz.names[i], new))  # function is vectored, but still
					{
					new = c(new, which.rz.names[i]);
					remaining = removeValueFromVector(which.rz.names[i], remaining);
					}
				}
			}

		
	
	
		
	  ts = rs+cs;
			ts.sort = sort(ts);
			ts.sort.names = names(ts.sort);
			n = length(ts.sort.names);
			if(n > 0)
				{
				for(i in 1:n)
					{
					if(!is.element(ts.sort.names[i], new))  # function is vectored, but still
						{
						new = c(new, ts.sort.names[i]);
						remaining = removeValueFromVector(ts.sort.names[i], remaining);
						}
					}
				}
	  ## shoult be good to go ... 
	  
	  new.idx = computeIndexFromOriginalToNew(original, new);
	  
	  
	 
	  info = list("zeroblock" = which.tz.names, "zerocols" = which.cz.names, "zerorows" = which.rz.names);
	  
	  # A.copy = A.copy[new.idx, ]; # update by rows
	  # A.copy = A.copy[, new.idx]; # update by cols
			
	# A.copy;
	
		A = A[new.idx, ]; # update by rows
		A = A[, new.idx]; # update by cols
		
		A = setAttribute("info", info, A);
			
	A;
	}
	
	
matrix.computeEigenRank = function(A, method="linear", norm="min-1", ...)
	{
	if(!is.square.matrix(A)) { stop("Matrix must be square, maybe multiply by transpose."); }
	
	A = matrix.convertMatrixToAdjacency(A);
	A = matrix.sortAdjacencyMatrix(A);
	A = matrix.addSuperNode(A);
	A = matrix.rowNormalize(A);
	
	if(method == "power")
		{
		# maybe create a "goal-seek" function that keeps multiplying the matrix until a threshold is met ...
		# A = matrixPowerConvergence(A, tol=0.0001);
	  # FOR BELOW:  user would have to pass in "pow = 22" or something ... 
		A = matrixPower(A, ...);	
		# should we consider "multiply.cpp" as option?
		} else {
				# block partition
				}
	
	A = matrix.removeSuperNode(A);
	
	A.vec = A[1,]; # any row
	if(norm == "min-1") { return( A.vec / min(A.vec) ); }
	if(norm == "max-100") { return ( 100 * A.vec / max(A.vec) ); }
	A.vec;
	}

