





#' matrix.rank
#'
#' @param A given matrix
#' @param ... Optional parameters to pass-through
#'
#' @return
#' @export
#'
#' @aliases rankMatrix matrixRank
matrix.rank = function(A, ...)
	{
  # matrixcalc is boring
  # Matrix might have some cool stuff ... sparse matrix
  # matrixStats is also boring
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



#' matrix.checkSquare
#'
#' @param A input matrix
#'
#' @return NA if not a matrix, TRUE/FALSE otherwise
#' @export
#'
#' @aliases is.square.matrix matrix.is.square
matrix.checkSquare = function(A)
	{
	if(!is.matrix(A)) { warning("Not a matrix"); return (NA); } # FALSE ?
	nr = nrow(A);
	nc = ncol(A);
	return (nr == nc);
	}

#' matrix.pow
#'
#'
#' Raise a matrix 'A' to a power 'pow' ... works for negatives if the inverse exists
#'
#' @param A a square matrix A
#' @param pow an integer power
#'
#' @return an updated square matrix A^pow
#' @export
#' @aliases powerMatrix matrixPower
#'
#' @examples
matrix.pow = function(A, pow)
	{
  # # should we consider "multiply.cpp" as option? ... larger matrices ... let's see in future ...
  ## check if square ???
  ## is.square.matrix
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


#' matrix.trace
#'
#' @param square a square matrix (n x n)
#'
#' @return numeric value for trace(matrix)
#' @export
#' @aliases traceMatrix matrixTrace
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' matrixTrace(m);#'
matrix.trace = function(square)
  {
  sum(diag(square));
  }


#' matrix.transpose
#'
#' @param mat a matrix
#'
#' @return matrix, updated
#' @export
#' @aliases transposeMatrix matrixTranspose
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' transposeMatrix(m);
#'
matrix.transpose = function(mat)
	{
	t(mat);
	}



#' matrix.rowNormalize
#'
#' @param A
#'
#' @return
#' @export
#'
#' @examples
matrix.rowNormalize = function(A)
	{
	A / rowSums(A);
	}

#' matrix.convertMatrixToAdjacency
#'
#' @param A
#' @param removeDiagonal
#' @param scaleNegatives
#' @param new.range
#'
#' @return
#' @export
#'
#' @examples
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









#' matrix.powerConvergence
#'
#' @param A
#' @param tol
#' @return
#' @export
#' @aliases matrix.matrixPowerConvergence 
#'
#' @examples
matrix.powerConvergence = function(A, tol=0.0001)
	{
	# maybe create a "goal-seek" function that keeps multiplying the matrix until a threshold is met ...
		# A = matrixPowerConvergence(A, tol=0.0001);
		
	# A = WWasr;  # owell.Rmd
	
	i = 1;
		A.copy = A;
			row.previous = A.copy[1,];
	i = 2;
		A.copy = A.copy %*% A;
			row.current  = A.copy[1,];
	
	n = length(row.previous);
	
	while( (mysum=sum(isClose(row.current,row.previous, tol=tol))) != n )
		{
		#cat("\n\n", " ...",i,"... \t sum: ", mysum, "\t ... n: ", n, "\n\n");
		A.copy = A.copy %*% A;
			row.previous = row.current;
			row.current  = A.copy[1,];
		i = 1 + i;
		}
		#cat("\n\n", " ...",i,"... \t sum: ", mysum, "\t ... n: ", n, "\n\n");
	
	info = list();
		info$tol = tol;
		info$iter = i;
		
	A.copy = setAttribute("info", info, A.copy);
	
	A.copy;
	}






#' matrix.sortAdjacencyMatrix
#'
#' @param A
#'
#' @return
#' @export
#'
#' @examples
matrix.sortAdjacencyMatrix = function(A)
	{
	# A = myA;
	
	original = remaining = colnames(A);
	new = c();
	# A.copy = A;

	cs = colSums(A);
	rs = rowSums(A);

	which.tz.names = "";
	tnew = c();
	if(FALSE)
		{
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
					tnew = c(tnew, which.tz.names[i]);
					remaining = removeValueFromVector(which.tz.names[i], remaining);
					}
				}
			}

		}
		

rnew = c();
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
					rnew = c(rnew, which.rz.names[i]);
					remaining = removeValueFromVector(which.rz.names[i], remaining);
					}
				}
			}

cnew = c();
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
					cnew = c(cnew, which.cz.names[i]);
					remaining = removeValueFromVector(which.cz.names[i], remaining);
					}
				}
			}


## final, what's remaining
fnew = c();
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
						fnew = c(fnew, ts.sort.names[i]);
						remaining = removeValueFromVector(ts.sort.names[i], remaining);
						}
					}
				}
	  



	  #info = list("zeroblock" = which.tz.names, "zerocols" = which.cz.names, "zerorows" = which.rz.names);
	  
	  rinfo = list("C1" = rnew, "C3" = cnew, "C2" = fnew);
	  # dissertation, pdf #79, pg 49

	  # A.copy = A.copy[new.idx, ]; # update by rows
	  # A.copy = A.copy[, new.idx]; # update by cols

	# A.copy;
	
		## shoult be good to go ...
		
		# added ...
		new = c(rnew, fnew, cnew);

	  new.idx = computeIndexFromOriginalToNew(original, new);
	  
	  sinfo = list("original" = original, "new" = new, "new.idx" = new.idx);

		A = A[new.idx, ]; # update by rows
		A = A[, new.idx]; # update by cols

		# A = setAttribute("info", info, A);
		A = setAttribute("rinfo", rinfo, A);
		A = setAttribute("sinfo", sinfo, A);
		A = setAttribute("dim", dim(A), A);

	A;
	}
	
	
# myV = c(0,0,0,0,0,0,0,0,0,0,
        # 0,0,0,0,0,0,0,0,0,0,
      	# 0,0,0,0,0,0,0,0,0,0,
      	# 0,1,1,0,0,0,0,0,0,0,
      	# 1,1,0,0,0,0,0,0,0,0,
      	# 0,0,0,0,0,0,0,0,0,0,
      	# 1,1,0,0,1,1,0,0,0,0,
      	# 1,0,0,1,0,1,0,0,0,0,
      	# 0,0,1,0,0,0,0,0,0,0,
      	# 0,0,0,0,0,1,0,1,0,0);

# myA = matrix(myV, nrow=10, byrow=TRUE);
  # rownames(myA) = colnames(myA) = paste0("P.",1:10);
# myA;
		# pi.lin = matrix.computeEigenRank(myA);
		# pi.pow = matrix.computeEigenRank(myA, method="power");
		### cor( pi.lin[sort(names(pi.lin))], pi.pow[sort(names(pi.pow))] );
		# cor(pi.lin$eigenrank, pi.pow$eigenrank);
	
	
#' matrix.computeEigenRank
#'
#' @param A
#' @param method
#' @param norm
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
matrix.computeEigenRank = function(A, method="linear")
	{
	# matrix.computeEigenRank = function(A, method="linear", norm="max-100", ...)
	
	if(!is.square.matrix(A)) 
		{ 
		stop("Matrix must be square, maybe multiply by transpose."); 
		}	
	
	# A = myA;
		A.copy = A;
		A.copy = matrix.convertMatrixToAdjacency(A.copy);
	Ainfo = list(); 
	Ainfo$sum = sum(A.copy);
	Ainfo$dim = dim(A.copy);
	Ainfo$n = Ainfo$dim[1];
	Ainfo$density = Ainfo$sum / prod(Ainfo$dim);  # sparseness
		
		A.copy = matrix.sortAdjacencyMatrix(A.copy);
			A.attributes = getAllAttributes(A.copy);
				c1 = length(A.attributes$rinfo$C1);
				c2 = length(A.attributes$rinfo$C2);
				c3 = length(A.attributes$rinfo$C3);
			members = list( "c1" = 1:c1, "c2" = (1+c1):(c1+c2), "c3" = (1+c1+c2):(c1+c2+c3) );
			members.names = c( rep("c1", c1), rep("c2", c2), rep("c3", c3) );
		# we lose the attributes here ... 
		A.copy = matrix.addSuperNode(A.copy);
	#Ainfo$sum.super = sum(A.copy);
		A.copy = matrix.rowNormalize(A.copy);		
	#Ainfo$sum.norm = sum(A.copy);
		
		pinfo = list();
		
		
	if(method == "power")
		{
		# maybe create a "goal-seek" function that keeps multiplying the matrix until a threshold is met ...
		# A = matrixPowerConvergence(A, tol=0.0001);
		
		
		# should we consider "multiply.cpp" as option?
		# maybe caching / parallelizing .... 
		
		# FOR BELOW:  user would have to pass in "pow = 22" or something into the MAIN of this function ...
		##### A.copy = matrixPower(A.copy, ...);		
		
		A.pow = matrix.powerConvergence(A.copy);
			pinfo = getAttribute("info", A.pow);
		A.pow = matrix.removeSuperNode(A.pow);
		A.vec = A.pow[1,]; # any row	
		
		} else {
				# TODO ...
				# let's just solve without partitioning ?
				# A.inv = solve(WWasr);  # owell.Rmd
				# v = A.inv[,24] * A.inv[24,]

				# block partition
				# pg. 166 is example of computation
				# pg. 80 [EQN 3.6 ... 3.10]  explains the why
				# I think P6 is wrong in dissertation 
				# http://www.mshaffer.com/arizona/dissertation/mjsPRINT.pdf
				
				# A = P.s[2:11,2:11]  # 2020-11-27_imdb-graph-ranks.Rmd
				
				
				
				
				
								
				# A.copy[1:10,1:10]
				
				# blocks = c(c1,c1+c2,c1+c2+c3);
				
				Qbar.t = A.copy[(1+c1):(c1+c2), (1):(c1)];				
				Rbar.t = A.copy[(1+c1):(c1+c2), (1+c1):(c1+c2)];				
				Sbar.t = A.copy[(1+c1+c2):(c1+c2+c3), (1):(c1)];
				Tbar.t = A.copy[(1+c1+c2):(c1+c2+c3), (1+c1):(c1+c2)];
				
				Rbar = matrix.transpose(Rbar.t);
				Tbar = matrix.transpose(Tbar.t);
				
							
				LHS = diag(1,nrow=c2) - Rbar;				
					one.col.c2 = matrix(1, ncol=1, nrow = c2);
				RHS = one.col.c2 + Tbar %*% one.col.c2;				
				pi.C2 = solve(LHS,RHS);
				
				pi.C3 = rep(1, c3);
					
				Qbar = matrix.transpose(Qbar.t);
				Sbar = matrix.transpose(Sbar.t);
					one.col.c1 = matrix(1, ncol=1, nrow = c1);
				pi.C1 = one.col.c1 + Qbar %*% pi.C2 + Sbar %*% one.col.c2;
				
				A.vec = c(pi.C1, pi.C2, pi.C3);
					names(A.vec) = c(A.attributes$rinfo$C1, A.attributes$rinfo$C2, A.attributes$rinfo$C3);
					
				# does this make it the same as power?	
				A.vec = A.vec / (Ainfo$n + sum(A.vec) );
				
				}

	
	A.df = as.data.frame( cbind( names(A.vec), as.numeric(A.vec), members.names ) );
		colnames(A.df) = c("nodes", "eigenrank", "members");
	A.df$eigenrank = as.numeric(A.df$eigenrank);	
		rownames(A.df) = A.df$nodes;
	A.df$eigenrank.min1 = A.df$eigenrank / min(A.df$eigenrank);
	A.df$eigenrank.max100 = 100* A.df$eigenrank / max(A.df$eigenrank);
	
		A.df = setAttribute("method", method, A.df);
		A.df = setAttribute("ainfo", Ainfo, A.df);
		A.df = setAttribute("pinfo", pinfo, A.df);
		A.df = setAttribute("cinfo", A.attributes, A.df);
		
	# A.vec = setAttribute("members", members, A.vec);
	
	# if(norm == "min-1") { return( A.vec / min(A.vec) ); }
	# if(norm == "max-100") { return ( 100 * A.vec / max(A.vec) ); }
	
	# A.vec;
	
	A.df;
	}




#' matrix.removeSuperNode
#'
#' @param A
#'
#' @return
#' @export
#'
#' @examples
matrix.removeSuperNode = function(A)
	{
	n = nrow(A) - 1;
	A[1:n,1:n];
	}

#' matrix.addSuperNode
#'
#' @param A
#' @param name
#'
#' @return
#' @export
#'
#' @examples
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
































# QUADRATIC FORM OF MATRIX ...
# http://math.ucdenver.edu/~esulliva/LinearAlgebra/SlideShows/07_02.pdf



# compute eigenRank here using the Romani / Del Corsi approach
# append super node to end, use inverse "solve" or ginv

# A = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0), .Dim = c(10L,  10L), .Dimnames = list(c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6",  "P.7", "P.8", "P.9", "P.10"), c("P.1", "P.2", "P.3", "P.4", "P.5",  "P.6", "P.7", "P.8", "P.9", "P.10")))

# A = structure(c(0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(10L,  10L), .Dimnames = list(c("P.1", "P.2", "P.3", "P.4", "P.5", "P.6",  "P.7", "P.8", "P.9", "P.10"), c("P.1", "P.2", "P.3", "P.4", "P.5",  "P.6", "P.7", "P.8", "P.9", "P.10")))













#' matrix.rotate
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#' @aliases rotateMatrix matrixRotate
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix(m);
#' rotateMatrix(m,FALSE);
#'
matrix.rotate = function(x,clockwise=TRUE)
	{
	if(clockwise)
		{
		t( apply(x, 2, rev) );
		} else 	{
				apply( t(x),2, rev);
				}
	}

#' matrix.rotate90
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#' @aliases rotateMatrix90 matrixRotate90
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix90(m);
#' rotateMatrix90(m,FALSE);
#'
matrix.rotate90 = function(x,clockwise=TRUE)
	{
  rotateMatrix(x,clockwise);
	}

#' matrix.rotate180
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#' @aliases rotateMatrix180 matrixRotate180
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix180(m);
#' rotateMatrix180(m,FALSE);  # equivalent
#'
matrix.rotate180 = function(x,clockwise=TRUE)
	{
  rotateMatrix(
    rotateMatrix(x,
			clockwise),
		clockwise);

	}

#' matrix.rotate270
#'
#' @param x matrix
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#' @aliases rotateMatrix270 matrixRotate270
#'
#' @examples
#' m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
#' rotateMatrix270(m);
#'
#' rotateMatrix270(m,FALSE);
#' rotateMatrix90(m); # equivalent
#'
matrix.rotate270 = function(x,clockwise=TRUE)
	{
  rotateMatrix(
    rotateMatrix(
      rotateMatrix(x,
			clockwise),
		clockwise),
	clockwise);
	}


#' matrix.rotateAngle
#'
#' @param x matrix
#' @param a angle, must be a multiple of 90 degrees, can be 630
#' @param clockwise direction of rotation, default is clockwise
#'
#' @return matrix, updated
#' @export
#' @aliases rotateMatrixAngle matrixRotateAngle
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
matrix.rotateAngle = function(x, a=0, clockwise=TRUE)
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
	
	