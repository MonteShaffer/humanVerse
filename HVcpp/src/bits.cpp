#include <Rcpp/Lightest> // Rcpp 1.0.8 or newer

//' Shift Bits to the Right
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
NumericVector cpp_RShift(const std::vector<long long int> arr, int b)
{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long unsigned int res = _RShift(element, b);
		r.push_back(res);
		}
	return r;
}
long long _RShift(long long a, int b) 
	{ 
	return a >> b;
	}

//' Shift Bits to the LEFT
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
NumericVector cpp_LShift(const std::vector<long long int> arr, int b)
{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long unsigned int res = _LShift(element, b);
		r.push_back(res);
		}
	return r;
}
long long _LShift(long long a, int b) 
	{ 
	return a << b;
	}


