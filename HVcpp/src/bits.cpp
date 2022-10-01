#include <Rcpp.h>
#include <vector>
#include <cmath>
using namespace Rcpp;

// built-in function seems to allow a, b to both be multivariate, some sort of recycling?
long long s_SHIFT_R(long long a, int b) 
	{ 
	return a >> b;
	}
	
	
//' Shift Bits to the Right
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
NumericVector cpp_SHIFT_R(const std::vector<long long int> arr, int b)
	{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long int res = s_SHIFT_R(element, b);
		r.push_back(res);
		}
	return r;
	}




long long s_SHIFT_L(long long a, int b) 
	{ 
	return a << b;
	}


//' Shift Bits to the LEFT
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
NumericVector cpp_SHIFT_L(const std::vector<long long int> arr, int b)
	{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long int res = s_SHIFT_L(element, b);
		r.push_back(res);
		}
	return r;
	}



// do OR, XOR, AND ... 
// https://www.geeksforgeeks.org/bitwise-operators-in-c-cpp/

long long s_AND(long long a, long long int b) 
	{ 
	return (a & b);
	}

//' Pairwise AND operations
//'
//' @param a INTEGER to be manipulated
//' @param b INTEGER to be manipulated
//' @return Updated INTEGER after AND
// [[Rcpp::export]]
NumericVector cpp_AND(const std::vector<long long int> arr, const std::vector<long long int> brr)
	{
	NumericVector r{};
	long long int i = 0;
	for (auto& element : arr) 
		{
		// pairwise 
		long long int res = s_AND(element, brr[i]);
		r.push_back(res);
		++i;
		}
	return r;
	}
	

	
long long s_OR(long long a, long long int b) 
	{ 
	return (a | b);
	}
	
	
//' Pairwise OR operations
//'
//' @param a INTEGER to be manipulated
//' @param b INTEGER to be manipulated
//' @return Updated INTEGER after OR
// [[Rcpp::export]]
NumericVector cpp_OR(const std::vector<long long int> arr, const std::vector<long long int> brr)
	{
	NumericVector r{};
	long long int i = 0;
	for (auto& element : arr) 
		{
		long long int res = s_OR(element, brr[i]);
		r.push_back(res);
		++i;
		}
	return r;
	}	
	
long long s_XOR(long long a, long long int b) 
	{ 
	return (a ^ b);
	}


//' Pairwise XOR operations
//'
//' @param a INTEGER to be manipulated
//' @param b INTEGER to be manipulated
//' @return Updated INTEGER after XOR
// [[Rcpp::export]]
NumericVector cpp_XOR(const std::vector<long long int> arr, const std::vector<long long int> brr)
	{
	NumericVector r{};
	long long int i = 0;
	for (auto& element : arr) 
		{
		long long int res = s_XOR(element, brr[i]);
		r.push_back(res);
		++i;
		}
	return r;
	}	 


long long s_NOT(long long a) 
	{ 
	return (~a);
	}
	
//' NOT operation
//'
//' @param a INTEGER to be manipulated
//' @return Updated INTEGER after NOT
// [[Rcpp::export]]
NumericVector cpp_NOT(const std::vector<long long int> arr)
	{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long int res = s_NOT(element);
		r.push_back(res);
		}
	return r;
	}	




	