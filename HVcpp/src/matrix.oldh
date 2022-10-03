#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;		// 'maps' rather than copies 
using Eigen::MatrixXd;	// variable size matrix, double precision
using Eigen::MatrixXcd;	// ABOVE, complex 
using Eigen::VectorXd;	// variable size vector, double precision
using Eigen::VectorXcd;	// ABOVE, complex 

// [[Rcpp::export]]
VectorXd matrix_diagonal(Map<MatrixXd> M)
	{
	VectorXd RES 	= M.diagonal();
	return RES;
	}
	
	
// [[Rcpp::export]]
VectorXcd matrix_diagonal_complex(const MatrixXcd& Mc)
	{
	VectorXcd RES 	= Mc.diagonal();
	return RES;
	}
	

// [[Rcpp::export]]
double matrix_trace(Map<MatrixXd> M)
	{
	VectorXd RES 	= M.diagonal();
	double res 		= RES.sum();	
	return res;
	}
	
/*
// [[Rcpp::export]]
std::complex<double> matrix_trace_complex(const MatrixXcd& Mc)
	{
	VectorXd RES 				= Mc.diagonal();
	std::complex<double> res 	= RES.sum();	
	return res;
	}
*/

/*
// [[Rcpp::export]]
std::complex<double> matrix_trace_complex(const MatrixXd& Re, const MatrixXd& Im)
	{
	MatrixXcd M 				= toComplex_(Re, Im);
	VectorXcd RES 				= M.diagonal();
	std::complex<double> res 	= RES.sum()
	return res;
	}
	
*/



/*

MatrixXcd toComplex_(const MatrixXd& Re, const MatrixXd& Im) 
{
	const std::complex<double> I_ {0.0, 1.0};
	return 		 Re.cast<std::complex<double>>() + 
			I_ * Im.cast<std::complex<double>>();
}


// [[Rcpp::export]]
VectorXcd matrix_diagonal_complex(const MatrixXd& Re, const MatrixXd& Im)
	{
	MatrixXcd Mc 	= toComplex_(Re, Im);
	VectorXcd RES 	= Mc.diagonal();
	return RES;
	}
*/	


/*
// m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
// mc = m + (3+2i)

m 	= as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
mc 	= m + (3+2i);

# as expected 
matrix_diagonal(m);
# [1] 1 3 5

# as expected, throws error regarding type 						
matrix_diagonal(mc); 
# Error in matrix_diagonal(mc) : Wrong R type for mapped matrix
# In addition: Warning message:
# In matrix_diagonal(mc) : imaginary parts discarded in coercion

# as expected with prep work( separating into buckets )				
matrix_diagonal_complex(Re(mc), Im(mc));	
# [1] 4+2i 6+2i 8+2i

# OBJECTIVE (direct call, preferably as MAP)						
matrix_diagonal_complex2(mc);

*/


/*
// [[Rcpp::export]]
VectorXcd matrix_diagonal_complex2(Map<MatrixXcd> M)
	{
	// MatrixXcd M 	= toComplex_(Re, Im);
	VectorXcd RES 	= M.diagonal();
	return RES;
	}
	
// [[Rcpp::export]]
VectorXcd matrix_diagonal_complex3(Rcpp::ComplexMatrix M)
	{
	// MatrixXcd M 	= toComplex_(Re, Im);
	VectorXcd RES 	= M.diagonal();
	return RES;
	}
*/	


/*

// [[Rcpp::export]]
double matrix_trace(Map<MatrixXd> M)
	{
	VectorXd RES 	= M.diagonal();
	double res 		= RES.sum();
	
	return res;
	}


// [[Rcpp::export]]
std::complex<double> matrix_trace_complex(const MatrixXd& Re, const MatrixXd& Im)
	{
	MatrixXcd M 				= toComplex_(Re, Im);
	VectorXcd RES 				= M.diagonal();
	std::complex<double> res 	= RES.sum()
	return res;
	}
*/


/*

ComplexVector 

https://stackoverflow.com/a/40322060/184614

So for a single complex number you (still) pass a ComplexVector which happens to be of length one.

 And if you really want a scalar, you can get it too:
*/

/*
VectorXcd matrix_diagonal_complex(Map<MatrixXd> Re, Map<MatrixXd> Im)
	{
	Map<MatrixXcd> M = matricesToMatrixXcd(Re, Im);
	VectorXcd RES = M.diagonal();
	return RES;
	}
*/	

// m = as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
// mc = m + (3+2i)

	
/*	
// [[Rcpp::export]]
SEXP matrix_diagonal_complex(const Eigen::MatrixXd& Re,
								const Eigen::MatrixXd& Im) 
	{
	const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
	Eigen::MatrixXd RES = M.diagonal();
	return Rcpp::wrap(RES);
	}
	
*/

// https://gist.github.com/pshriwise/67c2ae78e5db3831da38390a8b2a209f
/*
template<typename _Matrix_Type_>
_Matrix_Type_ pseudoInverse(const _Matrix_Type_ &a, double epsilon = std::numeric_limits<double>::epsilon())
{

	Eigen::JacobiSVD< _Matrix_Type_ > svd(a ,Eigen::ComputeFullU | Eigen::ComputeFullV);
        // For a non-square matrix
        // Eigen::JacobiSVD< _Matrix_Type_ > svd(a ,Eigen::ComputeThinU | Eigen::ComputeThinV);
	double tolerance = epsilon * std::max(a.cols(), a.rows()) *svd.singularValues().array().abs()(0);
	return svd.matrixV() *  (svd.singularValues().array().abs() > tolerance).select(svd.singularValues().array().inverse(), 0).matrix().asDiagonal() * svd.matrixU().adjoint();
}
*/


// // https://eigen.tuxfamily.org/dox/classEigen_1_1MatrixBase.html#a7ad8f77004bb956b603bb43fd2e3c061
// LOTS of good functions, check if inverse exists ... 	
// is_sparse is important 
// lots of matrix.isSparse = f in R before calling C++
// see EigenR.R file 
// 


/*

	

// [[Rcpp::export]]
SEXP matrix_rank(Eigen::MatrixXd A)
	{
	Eigen::MatrixXd C = A.transpose();  // lot's of rank functions 
	return Rcpp::wrap(C);
	}
	
	
// [[Rcpp::export]]
SEXP matrix_transpose(Eigen::MatrixXd A)
	{
	Eigen::MatrixXd C = A.transpose();
	return Rcpp::wrap(C);
	}

// [[Rcpp::export]]
SEXP matrix_multiplication(Eigen::MatrixXd A, Eigen::MatrixXd B)
	{
	Eigen::MatrixXd C = A * B;
	return Rcpp::wrap(C);
	}

// [[Rcpp::export]]
SEXP matrix_multiplication_map(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B)
	{
	Eigen::MatrixXd C = A * B;
	return Rcpp::wrap(C);
	}





// could we pass a method for RANKING?	
template <typename Number>
unsigned rank(const Eigen::Matrix<Number, Eigen::Dynamic, Eigen::Dynamic>& M) 
	{
	return M.colPivHouseholderQr().rank();
	}


// [[Rcpp::export]]
unsigned matrix_rank_real(const Eigen::MatrixXd& M) 
	{
	return rank<double>(M);
	}



// [[Rcpp::export]]
unsigned matrix_rank_complex(const Eigen::MatrixXd& Re,
								const Eigen::MatrixXd& Im) 
	{
	const Eigen::MatrixXcd M = matricesToMatrixXcd(Re, Im);
	return rank<std::complex<double>>(M);
	}
*/





































	
// [[Rcpp::export]]
MatrixXd matrix_add(Map<MatrixXd> A, Map<MatrixXd> B)
	{
	// Therefore, the instruction a = a.transpose() does not replace a with its transpose, as one would expect:
	return A + B;
	}
	
// [[Rcpp::export]]
MatrixXd matrix_subtract(Map<MatrixXd> A, Map<MatrixXd> B)
	{
	return A.transpose();
	}
	
// [[Rcpp::export]]
MatrixXd matrix_multiply(Map<MatrixXd> A, Map<MatrixXd> B)
	{
	//  Eigen treats matrix multiplication as a special case and takes care of introducing a temporary here, so it will compile m=m*m as:
	// tmp = m*m; m = tmp;

	return A.transpose();
	}






// [[Rcpp::export]]
MatrixXd matrix_multiplyN(MatrixXd& A, int n=2, int type=1)
	{	
	int nrow = A.rows();
	int ncol = A.cols();
	
	MatrixXd B;
		
	if(n == 0) 	{ return B.Identity(nrow, ncol);}
	if(n <  0) 	{ B = matrix_inverse(A, type);	}
	if(n >  0) 	{ B = A;						}
	
	/*
	int an = abs(n) - 1;
	for(i = 0; i < an; i++)
		{
		// B = B * B;
		}
		
	return B;  // literally R went away ... 
	*/
	return A;
	}



	
// [[Rcpp::export]]
MatrixXd matrix_determinant(Map<MatrixXd> A, int type=1)
	{
	// https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html
/* 
	if(type == 1) { return A.determinant(); 				}
	if(type == 2) { return A.PartialPivLU.determinant();	}
	if(type == 3) { return A.FullPivLU.determinant();		}
	 */ // const MatrixXd& A)
	return A.determinant();
	}
	
// [[Rcpp::export]]
MatrixXd matrix_inverse(Map<MatrixXd> A, int type=1, double tol=0)
	{
	// diagonal size times machine epsilon.
	// NumTraits<Scalar>::epsilon()
	// If you want to come back to the default behavior, call setThreshold(Default_t)
	// https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html
/* 
	if(type == 1) { return A.inverse(); 				}
	if(type == 2) { return A.PartialPivLU.inverse();	}
	if(type == 3) { return A.FullPivLU.inverse();		}
	 */ // const MatrixXd& A)
	return A.inverse();
	}
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;		// 'maps' rather than copies 
using Eigen::MatrixXd;	// variable size matrix, double precision
using Eigen::MatrixXcd;	// ABOVE, complex 
using Eigen::VectorXd;	// variable size vector, double precision
using Eigen::VectorXcd;	// ABOVE, complex 

// [[Rcpp::export]]
VectorXd matrix_diagonal(Map<MatrixXd> M)
	{
	return M.diagonal();
	}

// [[Rcpp::export]]
double matrix_trace(Map<MatrixXd> M)
	{
	return M.diagonal().sum();
	}

// [[Rcpp::export]]
MatrixXd matrix_transpose(Map<MatrixXd> M)
	{
	// Rcpp::sourceCpp("matrix.cpp", verbose=TRUE);
	// https://eigen.tuxfamily.org/dox/group__TutorialMatrixArithmetic.html
	// Therefore, the instruction a = a.transpose() does not replace a with its transpose, as one would expect:
	// For in-place transposition, as for instance in a = a.transpose(), simply use the transposeInPlace() function:
	// There is also the adjointInPlace() function for complex matrices.
	// return M.transpose();
	// return M.transposeInPlace();
	return M.transpose();  // I am not doing any additional STUFF
	}

	
// [[Rcpp::export]]
MatrixXd matrix_add(Map<MatrixXd> A, Map<MatrixXd> B)
	{
	return A + B;
	}
	
// [[Rcpp::export]]
MatrixXd matrix_subtract(Map<MatrixXd> A, Map<MatrixXd> B)
	{
	return A - B;
	}

// [[Rcpp::export]]
MatrixXd matrix_zero(int row=5, int col=-1 )
	{
	// https://eigen.tuxfamily.org/dox/group__TutorialAdvancedInitialization.html
	if(col <= -1) { col = row; }
	MatrixXd B;
	return B.Zero(row, col);
	}

	
// [[Rcpp::export]]
MatrixXd matrix_multiply(Map<MatrixXd> A, Map<MatrixXd> B)
	{
	return A * B;
	}	

// [[Rcpp::export]]
MatrixXd matrix_multiplyT(Map<MatrixXd> A)
	{
	return A * A.transpose();
	}
	


// [[Rcpp::export]]
MatrixXd matrix_identity(int size=5)
	{
	MatrixXd B;
	return B.Identity(size, size);
	}


/*

// [[Rcpp::export]]
MatrixXd matrix_determinant(Map<MatrixXd> A, int type=1)
	{
	// While inverse and determinant are fundamental mathematical concepts, in numerical linear algebra they are not as useful as in pure mathematics.
	/* 
	if(type == 1) { return A.determinant(); 				}
	if(type == 2) { return A.PartialPivLU.determinant();	}
	if(type == 3) { return A.FullPivLU.determinant();		}
	 */
	return A.determinant();
	}

// [[Rcpp::export]]
MatrixXd matrix_inverse(Map<MatrixXd> A, int type=1)
	{
	// diagonal size times machine epsilon.  // , double eps=0)
	// NumTraits<Scalar>::epsilon()
	// If you want to come back to the default behavior, call setThreshold(Default_t)
	// https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html
/* 
	if(type == 1) { return A.inverse(); 				}
	if(type == 2) { return A.PartialPivLU.inverse();	}
	if(type == 3) { return A.FullPivLU.inverse();		}
	 */ // const MatrixXd& A)
	return A.inverse();
	}









	

 	
	
	
	
// /********************** COMPLEX PAIRS **********************/	
	
	
// [[Rcpp::export]]
VectorXcd matrix_diagonal_complex(const MatrixXcd& Mc)
	{
	/* VectorXcd RES 	= Mc.diagonal();
	return RES; */
	return Mc.diagonal();
	}
	
// [[Rcpp::export]]
std::complex<double> matrix_trace_complex(const MatrixXcd& Mc)
	{
	/* // return MC.diagonal().sum() // doesn't seem to work ...
	VectorXcd RES 					= Mc.diagonal();
	std::complex<double> res 		= RES.sum();	
	return res; */
	return Mc.diagonal().sum();
	}

// [[Rcpp::export]]
MatrixXcd matrix_transpose_complex(const MatrixXcd& Mc)
	{
	// https://eigen.tuxfamily.org/dox/group__TutorialMatrixArithmetic.html
	// Therefore, the instruction a = a.transpose() does not replace a with its transpose, as one would expect:
	// For in-place transposition, as for instance in a = a.transpose(), simply use the transposeInPlace() function:
	// There is also the adjointInPlace() function for complex matrices.
	// return Mc.transpose();
	// return Mc.adjointInPlace();
	return Mc.transpose();  // I am not doing any additional STUFF
	}
	
	
// [[Rcpp::export]]
MatrixXcd matrix_add_complex(const MatrixXcd& Ac, const MatrixXcd& Bc)
	{
	return Ac + Bc;
	}
	
// [[Rcpp::export]]
MatrixXcd matrix_subtract_complex(const MatrixXcd& Ac, const MatrixXcd& Bc)
	{
	return Ac - Bc;
	}
	
	

// [[Rcpp::export]]
MatrixXcd matrix_zero_complex(int row=5, int col=-1 )
	{
	// https://eigen.tuxfamily.org/dox/group__TutorialAdvancedInitialization.html
	if(col <= -1) { col = row; }
	MatrixXcd B;
	return B.Zero(row, col);
	}
	
	
// [[Rcpp::export]]
MatrixXcd matrix_multiply_complex(const MatrixXcd& Ac, const MatrixXcd& Bc)
	{
	return Ac * Bc;
	}		

// [[Rcpp::export]]
MatrixXcd matrix_multiplyT_complex(const MatrixXcd& Ac)
	{
	return Ac * Ac.transpose();
	}
	

// [[Rcpp::export]]
MatrixXcd matrix_identity_complex(int size=5)
	{
	MatrixXcd B;
	return B.Identity(size, size);
	}

	
// [[Rcpp::export]]
MatrixXcd matrix_determinant(const MatrixXcd& Ac, int type=1)
	{
	// While inverse and determinant are fundamental mathematical concepts, in numerical linear algebra they are not as useful as in pure mathematics.
	/* 
	if(type == 1) { return Ac.determinant(); 				}
	if(type == 2) { return Ac.PartialPivLU.determinant();	}
	if(type == 3) { return Ac.FullPivLU.determinant();		}
	 */
	return Ac.determinant();
	}


// [[Rcpp::export]]
MatrixXcd matrix_inverse_complex(const MatrixXcd& Ac, int type=1)
	{
	// https://eigen.tuxfamily.org/dox/group__TutorialLinearAlgebra.html
/* 
	if(type == 1) { return Ac.inverse(); 				}
	if(type == 2) { return Ac.PartialPivLU.inverse();	}
	if(type == 3) { return Ac.FullPivLU.inverse();		}
	 */ // const MatrixXd& A)
	return Ac.inverse();
	}
	


 	
 
