// [[Rcpp::depends(RcppEigen)]]
// #include <RcppEigen.h>



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
SEXP matrix_diagonal(Eigen::MatrixXd A)
	{
	Eigen::MatrixXd C = A.diagonal();
	return Rcpp::wrap(C);
	}
	

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

