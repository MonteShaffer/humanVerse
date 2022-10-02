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
	// VectorXd RES 	= M.diagonal();
	// return RES;
	return M.diagonal();
	}

// [[Rcpp::export]]
double matrix_trace(Map<MatrixXd> M)
	{
	//	VectorXd RES 	= M.diagonal();
	// double res 		= RES.sum();	
	// return res;
	return M.diagonal().sum();
	}

// [[Rcpp::export]]
MatrixXd matrix_transpose(Map<MatrixXd> M)
	{
	// MatrixXd RES = M.transpose();
	// return RES;
	return M.transpose();
	}

	
/********************** COMPLEX PAIRS **********************/	
	
	
// [[Rcpp::export]]
VectorXcd matrix_diagonal_complex(const MatrixXcd& Mc)
	{
	VectorXcd RES 	= Mc.diagonal();
	return RES;
	}
	
// [[Rcpp::export]]
std::complex<double> matrix_trace_complex(const MatrixXcd& Mc)
	{
	// return MC.diagonal().sum() // doesn't seem to work ...
	VectorXcd RES 					= Mc.diagonal();
	std::complex<double> res 		= RES.sum();	
	return res;
	}

// [[Rcpp::export]]
MatrixXcd matrix_transpose_complex(const MatrixXcd& Mc)
	{
	MatrixXcd RES = Mc.transpose();
	return RES;
	}
