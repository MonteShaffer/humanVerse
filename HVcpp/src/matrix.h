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

