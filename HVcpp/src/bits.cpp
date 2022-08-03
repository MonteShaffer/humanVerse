#include <Rcpp/Lightest> // Rcpp 1.0.8 or newer

//' Shift Bits to the Right
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
long long cpp_RShift(long long a, int b) 
	{ 
	return a >> b;
	}

//' Shift Bits to the LEFT
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
long long cpp_LShift(long long a, int b) 
	{ 
	return a << b;
	}


