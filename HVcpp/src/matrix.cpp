#include <RcppEigen.h>
// [[Rcpp::depends(RcppEigen)]]

using Eigen::Map;				// 'maps' rather than copies 
using Eigen::MatrixXd;			// variable size matrix, double precision
using Eigen::MatrixXcd;			// ABOVE, complex 
using Eigen::VectorXd;			// variable size vector, double precision
using Eigen::VectorXcd;			// ABOVE, complex 
using Eigen::FullPivLU;
using Eigen::PartialPivLU;


//  Rcpp::sourceCpp("matrix.cpp", verbose=TRUE);

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
MatrixXd matrix_identity(int size = 5)
	{
	MatrixXd B;
	return B.Identity(size, size);
	}

// [[Rcpp::export]]
MatrixXd matrix_inverse(Map<MatrixXd> A, int type = 1, double eps = 0)
	{
	
	
	if(type == 1) { return A.inverse(); }
	if(type == 2) 
		{ 
		PartialPivLU<MatrixXd> 	pplu;
		if(eps > 0) { pplu.setThreshold(eps); }		
		pplu.compute(A);
		return pplu(A).inverse();
		}
	if(type == 3) 
		{ 
		FullPivLU<MatrixXd> 	fplu;
		if(eps > 0) { fplu.setThreshold(eps); }		
		fplu.compute(A);
		return fplu(A).inverse();
		}

	return A.inverse();
	}


// make other functions cmatrix_diagonal ... etc. 

