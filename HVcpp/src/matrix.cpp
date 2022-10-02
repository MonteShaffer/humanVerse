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


double calculateTolerance(Map<MatrixXd> A, double new_factor, 
							bool compound=true, bool verbose=true)
{
	double eps 			= std::numeric_limits<double>::epsilon();
	int old_factor 		= A.diagonal().size(); 
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


// for SOLVE ... https://dirk.eddelbuettel.com/papers/RcppEigen-intro.pdf

// [[Rcpp::export]]
MatrixXd matrix_inverse(Map<MatrixXd> A, int type = 1, double tol_factor = -1, bool compound=true, bool verbose=true)
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

/*
// [[Rcpp::export]]
MatrixXd matrix_determinant(Map<MatrixXd> A, int type = 1, double tol_factor = -1, bool compound=true, bool verbose=true)
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

*/


// make other functions cmatrix_diagonal ... etc. 

