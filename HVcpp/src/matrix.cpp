#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

using Eigen::NumTraits;			// eigen prec/eps/measures 

using Eigen::Map;				// 'maps' rather than copies 
using Eigen::MatrixXd;			// variable size matrix, double precision
using Eigen::MatrixXcd;			// ABOVE, complex 
using Eigen::VectorXd;			// variable size vector, double precision
using Eigen::VectorXcd;			// ABOVE, complex 

using Eigen::PartialPivLU;						// type = 2
using Eigen::FullPivLU;							// type = 3

using Eigen::HouseholderQR;						// type = 4
using Eigen::ColPivHouseholderQR;				// type = 5
using Eigen::FullPivHouseholderQR;				// type = 6
using Eigen::CompleteOrthogonalDecomposition;	// type = 7

using Eigen::LLT;								// type = 8
using Eigen::LDLT;								// type = 9

// setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
// Rcpp::sourceCpp("matrix.cpp", verbose=TRUE);
// m 	= as.matrix(structure(c(1, 0, 4, 0, 3, 0, 2, 0, 5), .Dim = c(3L, 3L)));
// sourceCpp has a checksum caching mechanism ...
// based on file changes ... 

// [[Rcpp::export]]
VectorXd matrix_diagonal(Map<MatrixXd> M, int idx=0)
	{
	// *this is not required to be square.	
	return M.diagonal(idx);
	}

// [[Rcpp::export]]
double matrix_trace(Map<MatrixXd> M)
	{
	return M.diagonal().sum();
	}

// [[Rcpp::export]]
MatrixXd matrix_transpose(Map<MatrixXd> M)
	{
	// return M.transposeInPlace();
	return M.transpose();  
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
MatrixXd matrix_zero(int row = 5, int col = -1 )
	{
	if(col < 0) { col = row; }
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
MatrixXd matrix_identity(int size = 5)
	{
	MatrixXd B;
	return B.Identity(size, size);
	}



// [[Rcpp::export]]
double get_epsilon()
{
	// return std::numeric_limits<double>::epsilon();
	return NumTraits<double>::epsilon();
}

// [[Rcpp::export]]
double get_precision()
{
	return NumTraits<double>::dummy_precision();	
}

// [[Rcpp::export]]
double get_digits10()
{
	return NumTraits<double>::digits10();	
}



double calculateTolerance(Map<MatrixXd> A, double new_factor, 
							bool compound = true, bool verbose = true)
{
	double eps 			= get_epsilon();
	int old_factor 		= A.diagonalSize(); 
	std::string msg 	= "RTM: *matrix eps* it is defined as 'diagonal size times machine epsilon' \n\t\t\t\t[ setThreshold( ... NumTraits<Scalar>::epsilon() ]";
	std::string more 	= ""; 
	if(compound) { more = " * " + std::to_string(old_factor); }
		
	
	new_factor 		= fabs(new_factor); // has to be unsigned ... 
	
	double eps_old	= eps * old_factor;
	
	double eps_new 	= (compound) 	? eps_old 	* new_factor 
									: eps 		* new_factor;
	
	
// https://teuder.github.io/rcpp4everyone_en/060_printing_massages.html
	if(verbose)
		{
		Rcpp::Rcout << "\t\t\t" << msg << "\n\n";
		
		Rcpp::Rcout << "\tThe CPP tolerance [machine eps] : \t\t\t\t" << 
												eps << "\n\n";
		
		
		Rcpp::Rcout << "\tThe old tolerance factor was : \t\t\t\t\t" << 
												old_factor << "\n";
		Rcpp::Rcout << "\tBefore, the tolerance for" << 
						" this matrix operation *was* : \t" << 
												eps_old << "\n\n";
				
				
		Rcpp::Rcout << "\tBased on your input, the " << 
								"new tolerance factor : \t\t" << 
								new_factor << more << "\n";
										
		Rcpp::Rcout << "\tNow, the new tolerance for" << 
								" this matrix operation *is* : \t" << 
												eps_new << "\n\n";
		
		}	
	return eps_new;
}




// [[Rcpp::export]]	
bool is_symmetric(Map<MatrixXd> M, double prec = -1)
	{
	if(prec <= 0) { return M.isApprox( M.transpose() );	}
	// update TOLERANCE ??? 
	// prec vs tolerance ... 
	// A dummy_precision() function returning a weak epsilon value. It is mainly used as a default value by the fuzzy comparison operators.
	// https://stackoverflow.com/a/71827895/184614
	return M.isApprox( M.transpose(), prec );		
	}


	
// [[Rcpp::export]]	
bool is_positive_semi_definite(Map<MatrixXd> M, double prec = -1)
	{
	bool symm = is_symmetric(M, prec);
	if(!symm) { return false; }
	
	// need to be "selfAdjuoinded"?

	LDLT<MatrixXd> 	ldlt(M); // constructor 
	// https://stackoverflow.com/a/71827895/184614
	// npd = as.matrix(structure(c(1, 2, 2, 3), .Dim = c(2L, 2L)));
	// psd = as.matrix(structure(c(1, 2, 2, 3), .Dim = c(2L, 2L)));
	// two = as.matrix(structure(c(2, 2, 2, 2), .Dim = c(2L, 2L)));

	// not getting the answer ... 
	// https://scicomp.stackexchange.com/a/26223
	if (ldlt.info() == Eigen::NumericalIssue || !ldlt.isPositive()) 
		{
		return false;
		}
		
	return true;
	}
		
// [[Rcpp::export]]	
bool is_positive_definite(Map<MatrixXd> M, double prec = -1)
	{
	bool symm = is_symmetric(M, prec);
	if(!symm) { return false; }

	LDLT<MatrixXd> 	ldlt(M); // constructor 
	
	return ldlt.isPositive();
	}
	
	
	 


// [[Rcpp::export]]
MatrixXd matrix_inverse(Map<MatrixXd> A, int type = 1, double tol_factor = -1, bool compound = true, bool verbose = true)
	{
	if(type == 1) { return A.inverse(); }
	if(type == 2)
		{
		PartialPivLU<MatrixXd> 	pplu(A); // constructor 
		if(tol_factor > 0) 
			{
			double eps = calculateTolerance(A, tol_factor, 
											compound, verbose);
			// pplu.setThreshold(eps);
			}
		return pplu.inverse();
		}
	if(type == 3) 
		{ 
		FullPivLU<MatrixXd> 	fplu(A); // constructor 
		if(tol_factor > 0) 
			{
			double eps = calculateTolerance(A, tol_factor, 
											compound, verbose);
			// fplu.setThreshold(eps);
			}
		return fplu.inverse(); 
		}
 
	return A.inverse();
	}

// [[Rcpp::export]]
double matrix_determinant(Map<MatrixXd> A, int type = 1, double tol_factor = -1, bool compound=true, bool verbose=true)
	{
	if(type == 1) { return A.determinant(); }
	if(type == 2)
		{
		PartialPivLU<MatrixXd> 	pplu(A); // constructor 
		if(tol_factor > 0) 
			{
			double eps = calculateTolerance(A, tol_factor, 
											compound, verbose);
			// pplu.setThreshold(eps);
			}
		return pplu.determinant();
		}
	if(type == 3) 
		{ 
		FullPivLU<MatrixXd> 	fplu(A); // constructor 
		if(tol_factor > 0) 
			{
			double eps = calculateTolerance(A, tol_factor, 
											compound, verbose);
			// fplu.setThreshold(eps);
			}
		return fplu.determinant(); 
		}
 
	return A.determinant();
	}




// [[Rcpp::export]]
MatrixXd matrix_multiplyN(Map<MatrixXd> A, int n=2, int type = 1, double tol_factor = -1, bool compound = true, bool verbose = true)
	{	
	int nrow 	= A.rows();
	int ncol 	= A.cols();
	int an 		= abs(n) - 1;
	
	MatrixXd B;
		
	if(n == 0) 	{ return B.Identity(nrow, ncol);		}
	if(n <  0) 	{ B = matrix_inverse(A, type, 
						tol_factor, compound, verbose);	}
	if(n >  0) 	{ B = A;								}
	
	for(int i = 0; i < an; i++)
		{
		B = B * B;
		}		
	return B;
	}



/* 
 

*/


/*
void c___________ommments____() {} 




// install.packages

// TODO ... rank(M) ... eigen(M) ... solve(A, B) 
// linear-solver types, wrapped into one function ...
// maybe do ... fixed.intercept herein ... so column of 1's not needed
// if fixed.int , subtract fixed.intercept from given y ... 
// multinomial? ... return NAMED list ... 

// ... svd(M) ... U,V,S everything




// make other functions cmatrix_diagonal ... etc. 

NumTraits<Real>::epsilon(); }
   EIGEN_DEVICE_FUNC EIGEN_CONSTEXPR
   static inline Real dummy_precision() { return NumTraits<Real>::dummy_precision(); }
   EIGEN_DEVICE_FUNC EIGEN_CONSTEXPR
   static inline int digits10() { return NumTraits<Real>::digits10(); }
   *
   */
   
   
  // for SOLVE ... https://dirk.eddelbuettel.com/papers/RcppEigen-intro.pdf

/*
https://eigen.tuxfamily.org/dox/group__TopicLinearAlgebraDecompositions.html

type ... decomposition types 

type == 1 is default  Matrix::inverse() ... base forumula  

2 .... PartialPivLU	Invertible	Fast	Depends on condition number	-	-	Yes	Excellent	
Blocking, Implicit MT

3 ... FullPivLU	-	Slow	Proven	Yes	-	Yes	Excellent	
-

4 ... HouseholderQR	-	Fast	Depends on condition number	-	Orthogonalization	Yes	Excellent	
Blocking

5 ... ColPivHouseholderQR	-	Fast	Good	Yes	Orthogonalization	Yes	Excellent	
-

6 ... FullPivHouseholderQR	-	Slow	Proven	Yes	Orthogonalization	Yes	Average	
-

7 ... CompleteOrthogonalDecomposition	-	Fast	Good	Yes	Orthogonalization	Yes	Excellent	
-

8 ... LLT	Positive definite	Very fast	Depends on condition number	-	-	Yes	Excellent	
Blocking

9 ... LDLT

Rank revealing ... 3, 5, 6, 7 


svd/solvers types ... start at 21



21 ... BDCSVD (divide & conquer)	-	One of the fastest SVD algorithms	Excellent	Yes	Singular values/vectors, least squares	Yes (and does least squares)	Excellent	
Blocked bidiagonalization

22 ... JacobiSVD (two-sided)	-	Slow (but fast for small matrices)	Proven3	Yes	Singular values/vectors, least squares	Yes (and does least squares)	Excellent	
R-SVD

23 ... SelfAdjointEigenSolver	Self-adjoint	Fast-average2	Good	Yes	Eigenvalues/vectors	-	Excellent	
Closed forms for 2x2 and 3x3

24 ... ComplexEigenSolver	Square	Slow-very slow2	Depends on condition number	Yes	Eigenvalues/vectors	-	Average	
-

25 ... EigenSolver	Square and real	Average-slow2	Depends on condition number	Yes	Eigenvalues/vectors	-	Average	
-

26 ... GeneralizedSelfAdjointEigenSolver	Square	Fast-average2	Depends on condition number	-	Generalized eigenvalues/vectors	-	Good	
-


*/