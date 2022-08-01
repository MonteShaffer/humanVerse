#include <Rcpp/Lightest> // Rcpp 1.0.8 or newer

// [[Rcpp::export]]
long long cpp_RShift(long long a, int b) 
	{ 
	return a >> b;
	}

// [[Rcpp::export]]
long long cpp_LShift(long long a, int b) 
	{ 
	return a << b;
	}

