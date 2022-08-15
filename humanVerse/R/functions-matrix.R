

## eigen 
## https://eigen.tuxfamily.org/dox/group__SVD__Module.html
## two flavors of SVD 
## GINV, INV, SVD, TRANSPOSE, RANK, ... 
## https://stackoverflow.com/questions/31041921/how-to-get-rank-of-a-matrix-in-eigen-library
## https://stattrek.com/matrix-algebra/matrix-rank
## TAKE eigen ... 
## if(library(MATRIX), loop over RANK options, report all results
## rankMatrix <- function(x, tol = NULL,
##                      method = c("tolNorm2", "qr.R", "qrLINPACK", "qr",
##                                  "useGrad", "maybeGrad"),

## https://stackoverflow.com/questions/13033694/eigen-convert-dense-matrix-to-sparse-one

## matrix.isDense ... density as %, nonzero ~= nrows or ncols 
## https://eigen.tuxfamily.org/dox/group__SparseQuickRefPage.html



# https://cmdlinetips.com/2019/05/introduction-to-sparse-matrices-in-r/
# print(object.size(mat),units="auto")

# https://stackoverflow.com/questions/53307953/speed-up-sparse-matrix-multiplication-in-r
# tic/toc ... timer
# https://github.com/collectivemedia/tictoc
# In addition, this package provides classes Stack (implemented as a vector) and List (implemented as a list), both of which support operations push, pop, first, last, clear and size.
# build mine like tic/toc but using nanotime ... microtime ...


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
  # matrixcalc is boring ... svd.inverse
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
	stop(paste0("I need a matrix library for this!","\n\t\t","Matrix","\n\t\t","matrixcalc","\n"));
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
		A = convertFromOldRangeToNew(A, newrange=new.range); # oldrange is figured out automatically
			# this makes it a positive matrix, yet the action is a nonlinear transformation?
		}
	if(removeDiagonal)
		{
		diag(A) = 0;  # this is a weird assignment approach ...
		}
	A;
	}





matrix.computeDensity = function(A)
	{
	# sparsity is density

	# sum(A) / prod(dim(A));	# if all ones, this works

	length(which(A != 0)) / prod(dim(A));
	}



matrix.convertTypeByDensity = function(A, format="col", sparse.tol = 0.2)
	{
	info = list();
	info$density 	= matrix.computeDensity(A);  	# density (sparsity)
	info$o.class 	= class(A);						# original class

	if(info$density < sparse.tol)
		{
		A = matrix.convertToSparse(A, format);  # csc format
		info$n.class 	= class(A);
		}

	A = setAttribute("matrix.type", info, A);
	A;
	}


matrix.restoreType = function(A, info=NULL)
	{
	if(is.null(info)) { info = getAttribute("matrix.type", A); }

	if(!is.null(info))
		{
		if(exists("o.class", info))
			{
			if(is.element("dgCMatrix", info$o.class))
				{
				A = matrix.convertToSparse(A, "dgCMatrix");
				}
			if(is.element("dgRMatrix", info$o.class))
				{
				A = matrix.convertToSparse(A, "dgRMatrix");
				}
			if(is.element("matrix", info$o.class))
				{
				A = matrix.convertFromSparse(A);
				}
			}
		}

	A = deleteAttribute("matrix.type", A);  # as-if it never happened
	A;
	}


matrix.solve = function(A, ginv=FALSE)
	{
	# wrap in tryCatch, return NULL
	# # maybe use ginv? ... this is the same as svd.inverse ??? Moore-Penrose
	## MASS::ginv

	solve(A);
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
matrix.pow = function(A, pow, ...)
	{
  # # should we consider "multiply.cpp" as option? ... larger matrices ... let's see in future ...
  ## check if square ???
  ## is.square.matrix

	nr = nrow(A);
	pow = as.integer(pow);

	if(pow == 0) { return( diag(1, nr) ); }
	if(pow == 1) { return( A ); }

	A = matrix.convertTypeByDensity(A, ...);
		A.info = getAttribute("matrix.type", A);

	if(pow < 0)
		{
		A.inv = matrix.solve(A);
		if(is.null(A.inv))
			{
			stop("A is not invertible");
			}
		A.copy = A.inv;

		if(pow < -1)
			{
			for(i in 2:(-pow))
				{
				A.copy = A.copy %*% A.inv;
				}
			}
		}

	if(pow > 0)
		{
		A.copy = A;
		if(pow > 1)
			{
			for(i in 2:(pow))
				{
				A.copy = A.copy %*% A;
				}
			}
		}


	A.copy = matrix.restoreType(A.copy, A.info);

	A.copy;
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
matrix.powerConvergence = function(A, tol=0.0001, max.iter=100, ...)
	{
	# maybe create a "goal-seek" function that keeps multiplying the matrix until a threshold is met ...
		# A = matrixPowerConvergence(A, tol=0.0001);

	# A = WWasr;  # owell.Rmd

	A = matrix.convertTypeByDensity(A, ...);
		A.info = getAttribute("matrix.type", A);

	i = 1;
		A.copy = A;
			row.previous = A.copy[1,];
	i = 2;
		A.copy = A.copy %*% A;
			row.current  = A.copy[1,];

	n = length(row.previous);
	info = list();

	while( (mysum=sum(isClose(row.current,row.previous, tol=tol))) != n )
		{
		#cat("\n\n", " ...",i,"... \t sum: ", mysum, "\t ... n: ", n, "\n\n");
		A.copy = A.copy %*% A;
			row.previous = row.current;
			row.current  = A.copy[1,];
		i = 1 + i;
		if(i > max.iter) { info$max.stop = TRUE; break; }
		}
		#cat("\n\n", " ...",i,"... \t sum: ", mysum, "\t ... n: ", n, "\n\n");


		info$tol = tol;
		info$iter = i;

	A.copy = matrix.restoreType(A.copy, A.info);

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

	n = nrow(A);
		if(is.null( colnames(A) ) ) { colnames(A) = rownames(A) = 1:n; }

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

# https://stackoverflow.com/questions/1167448/most-mature-sparse-matrix-package-for-r

	# dgCMatrix ... double-sparse stored in CSC "Compressed Sparse Column"
matrix.convertToSparse = function(A, method="col")
	{
	switch( method,
		"col" 				= as(A, "dgCMatrix"),
		"csc" 				= as(A, "dgCMatrix"),
		"dgCMatrix" 		= as(A, "dgCMatrix"),
		"row" 				= as(A, "dgRMatrix"),
		"csr" 				= as(A, "dgRMatrix"),
		"dgRMatrix" 		= as(A, "dgRMatrix"),
		as(A, "dgCMatrix")  # default
		);
	}

# A.csc = matrix.convertToSparse(A, "col");

matrix.convertFromSparse = function(A)
	{
	as(A, "matrix");
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
# myA; myA.copy = myA;

		# pi.lin = matrix.computeEigenRank(myA);
		# pi.pow = matrix.computeEigenRank(myA, method="power");
		### cor( pi.lin[sort(names(pi.lin))], pi.pow[sort(names(pi.pow))] );
		# cor(pi.lin$eigenrank, pi.pow$eigenrank);

## myA.copy = myA; myA.c1c2c3 = myA.copy; myA.c1c2c3;
## myA.copy = myA; myA.copy[1,8]=1; myA.copy[2,6]=1; myA.copy[3,9]=1; myA.copy[6,1]=1; myA.c2c3 = myA.copy;	myA.c2c3;
## myA.c2 = myA.c2c3; myA.c2[5,10] = 1; myA.c2[7,10] = 1; myA.c2[8,7] = 1; myA.c2;
## myA.c1c3 = matrix(0, nrow=10, ncol=10); myA.c1c3[3,8] = 1; myA.c1c3[4,8] = 1; myA.c1c3[5,9] = 1; myA.c1c3[6,10] = 1; myA.c1c3;
## myA.c1c2 = myA.c1c2c3; myA.c1c2[6,10] = 1; myA.c1c2[8,9] = 1; myA.c1c2[10,7] = 1; myA.c1c2;

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

	if(!matrix.checkSquare(A))
		{
		stop("Matrix must be square, maybe multiply by transpose.");
		}

	# A = WW;
	# A = myA;
		A.copy = A;
		A.copy = matrix.convertMatrixToAdjacency(A.copy);
	Ainfo = list();
	Ainfo$sum = sum(A.copy);
	Ainfo$dim = dim(A.copy);
	Ainfo$n = Ainfo$dim[1];
	Ainfo$SuperNode = Ainfo$n;
	Ainfo$density = Ainfo$sum / prod(Ainfo$dim);  # sparseness

		A.copy = matrix.sortAdjacencyMatrix(A.copy);
			A.attributes = getAllAttributes(A.copy);
				c1 = length(A.attributes$rinfo$C1);
				c2 = length(A.attributes$rinfo$C2);
				c3 = length(A.attributes$rinfo$C3);
			members = list( "c1" = 1:c1, "c2" = (1+c1):(c1+c2), "c3" = (1+c1+c2):(c1+c2+c3) );
			members.names = c( rep("c1", c1), rep("c2", c2), rep("c3", c3) );
		# we lose the attributes here ...
		# is sum super the same as attribute SuperNode (added 06/21/2021)
		A.copy = matrix.addSuperNode(A.copy);
	#Ainfo$sum.super = sum(A.copy);
		A.copy = matrix.rowNormalize(A.copy);
	#Ainfo$sum.norm = sum(A.copy);

		pinfo = list();

	if(c1 == 0 && c2 == 0 && c3 == 0)
		{
		stop("The matrix is empty! Won't work!");
		}

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
			Ainfo$SuperNode = getAttribute("SuperNode", A.pow);
		A.vec = A.pow[1,]; # any row

		A.vec.pow = A.vec;

		} else {
				# TODO ...
				# let's just solve without partitioning ?
				# A.inv = solve(WWasr);  # owell.Rmd
				# v = A.inv[,24] * A.inv[24,]
				# block partition
				# A = P.s[2:11,2:11]  # 2020-11-27_imdb-graph-ranks.Rmd
				# A.copy[1:10,1:10]
				# blocks = c(c1,c1+c2,c1+c2+c3);


				# pg. 166 is example of computation
				# pg. 80 [EQN 3.6 ... 3.10]  explains the why
				# I think P6 is wrong in dissertation
				# http://www.mshaffer.com/arizona/dissertation/mjsPRINT.pdf

				pi.C3 = rep(1, c3);
				if(length(pi.C3) == 0) { pi.C3 = NULL; }



								rc1 = (1+c1); 		cc1 = (c1);
								rc2 = (rc1+c2);		cc2 = (cc1+c2);
					if(c3 == 0) { rc2 = Ainfo$n + 1; }

				if(c2 != 0)
					{
					Qbar.t = as.matrix(A.copy[rc1:(rc2-1), (1):(cc1)]);
					Rbar.t = as.matrix(A.copy[rc1:(rc2-1), (1+cc1):(cc2)]);

					Qbar = matrix.transpose(Qbar.t);
					Rbar = matrix.transpose(Rbar.t);
					} else	{
							Qbar.t = Qbar = NULL;
							Rbar.t = Rbar = NULL;
							}

				if(rc2 > Ainfo$n)
					{
					Sbar.t = Sbar = NULL;
					Tbar.t = Tbar = NULL;
					} else 	{
							Sbar.t = as.matrix(A.copy[rc2:(Ainfo$n), (1):(cc1)]);
							Tbar.t = as.matrix(A.copy[rc2:(Ainfo$n), (1+cc1):(cc2)]);

							Sbar = matrix.transpose(Sbar.t);
							Tbar = matrix.transpose(Tbar.t);
							}

				one.col.c1 = matrix(1, ncol=1, nrow = c1);
				one.col.c2 = matrix(1, ncol=1, nrow = c2);

				if(c2 > 0)
					{
					# LHS = diag(1, nrow=c2) - Rbar;
					# RHS = one.col.c2 + Tbar %*% one.col.c2;
					LHS = diag(1, nrow = c2) - Rbar;
					if(!is.null(Tbar))
						{
						RHS = one.col.c2 + Tbar %*% matrix(1, nrow = dim(Tbar)[2], ncol = 1);
						} else	{
								RHS = one.col.c2;
								}

					pi.C2 = solve(LHS,RHS);
					} else { pi.C2 = NULL; }


				if(c1 > 0)
					{
					if(!is.null(pi.C2))
						{
						pi.C1 = one.col.c1 + Qbar %*% pi.C2;
						} else 	{
								pi.C1 = one.col.c1;
								if(!is.null(Qbar))
									{
									pi.C2.ones = matrix(1, nrow = dim(Qbar)[2], ncol = 1);

									pi.C1 = pi.C1 + Qbar %*% pi.C2.ones;
									}
								}

						if(!is.null(Sbar))
							{
							one.col.c2.ones = matrix(1, nrow = dim(Sbar)[2], ncol = 1);

							pi.C1 = pi.C1 + Sbar %*% one.col.c2.ones;
							}
					} else { pi.C1 = NULL;}

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
		SuperNode = A[n+1,n+1];
	A = A[1:n,1:n];
		A = setAttribute("SuperNode", SuperNode, A);
	A;
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




