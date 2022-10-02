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


double calculateTolerance(Map<MatrixXd> A, double new_factor, 
							compound=true, bool verbose=true)
{
	double eps 			= std::numeric_limits<double>::epsilon();
	double old_factor 	= eps * A.diagonal().size(); 
	std::string msg 	= "RTM: it is defined as 'diagonal size times machine epsilon' [ NumTraits<Scalar>::epsilon() ]";
			
	new_factor 		= fabs(new_factor); // has to be unsigned ... 
	
	double eps_old 	= eps * old_factor;
	double eps_new 	= (compound) ? eps * old_factor * new_factor 
								: eps * new_factor;
	
	
// https://teuder.github.io/rcpp4everyone_en/060_printing_massages.html
	if(verbose)
		{
		Rcpp::Rcout << "The old tolerance factor was : " << 
												old_factor << "\n";
		Rcpp::Rcout << "\t\t\t" << msg << "\n";
		Rcpp::Rcout << "The old tolerance was : " << 
												eps_old << "\n";
		
		
		Rcpp::Rcout << "You entered as a new tolerance factor : " << 
												new_factor << "\n";
		
		Rcpp::Rcout << "The system's eps [tolerance] is : " << 
												eps << "\n";
										
		Rcpp::Rcout << "Now, the new tolerance for" << 
						" this matrix operation is: " << 
												eps_new << "\n";
		
		}	
	return eps_new;
}


{
double old_factor, double new_factor, double eps = 0, const std::string msg="", bool compound=true, bool verbose=true)
	{
	if(eps == 0) { eps = std::numeric_limits<double>::epsilon(); }
	old_factor = fabs(old_factor); // has to be unsigned ... 
	new_factor = fabs(new_factor); // has to be unsigned ... 
	
	double eps_old = eps * old_factor;
	double eps_new = (compound) ? eps * old_factor * new_factor 
								: eps * new_factor;
	
// https://teuder.github.io/rcpp4everyone_en/060_printing_massages.html
	if(verbose)
		{
		Rcpp::Rcout << "The old tolerance factor was : " << 
												old_factor << "\n";
		Rcpp::Rcout << "\t\t\t" << msg << "\n";
		Rcpp::Rcout << "The old tolerance was : " << 
												eps_old << "\n";
		
		
		Rcpp::Rcout << "You entered as a new tolerance factor : " << 
												new_factor << "\n";
		
		Rcpp::Rcout << "The system's eps [tolerance] is : " << 
												eps << "\n";
										
		Rcpp::Rcout << "Now, the new tolerance for" << 
						" this matrix operation is: " << 
												eps_new << "\n";
		
		}	
	return eps_new;
	}

// [[Rcpp::export]]
MatrixXd matrix_inverse(Map<MatrixXd> A, int type = 1, double eps_factor = -1, bool compound=true, bool verbose=true)
	{
	if(type == 1) { return A.inverse(); }
	if(type == 2)
		{
		PartialPivLU<MatrixXd> 	pplu(A); // constructor 
		if(eps_factor > 0) 
			{
			double eps = calculateTolerance(eps_factor, compound, verbose);
			double eps = std::numeric_limits<double>::epsilon();
			// according to the docs ... 
			double eps_old = eps * A.diagonal().size(); 
			const std::string msg = "The documentation says it is defined as 'diagonal size times machine epsilon' [ NumTraits<Scalar>::epsilon() ]";
			e calculateTolerance(double old_factor, double new_factor, double eps = 0, const std::string msg="", bool compound=true, bool verbose=true)
	{
		eps_factor
			double eps = calculateTolerance(
		//if(eps > 0) { pplu.setThreshold(eps); }
		// pplu.setThreshold(1e-5);
		/*
		// diagonal size times machine epsilon.
	// NumTraits<Scalar>::epsilon()
	// If you want to come back to the default behavior, call setThreshold(Default_t)
	*/
		return pplu.inverse();
		}
	/* if(type == 2) 
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
 */
	return A.inverse();
	}


// make other functions cmatrix_diagonal ... etc. 

