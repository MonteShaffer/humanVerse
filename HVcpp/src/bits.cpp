#include <Rcpp.h>
#include <vector>
#include <cmath>
using namespace Rcpp;

// built-in function seems to allow a, b to both be multivariate, some sort of recycling?
long long b_RShift(long long a, int b) 
	{ 
	return a >> b;
	}
	
	
//' Shift Bits to the Right
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
NumericVector bits_RShift(const std::vector<long long int> arr, int b)
	{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long int res = b_RShift(element, b);
		r.push_back(res);
		}
	return r;
	}




long long b_LShift(long long a, int b) 
	{ 
	return a << b;
	}


//' Shift Bits to the LEFT
//'
//' @param a INTEGER to be manipulated
//' @param b int to shift
//' @return Updated INTEGER a appropriately shifted
// [[Rcpp::export]]
NumericVector bits_LShift(const std::vector<long long int> arr, int b)
	{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long int res = b_LShift(element, b);
		r.push_back(res);
		}
	return r;
	}



// do OR, XOR, AND ... 
// https://www.geeksforgeeks.org/bitwise-operators-in-c-cpp/

long long b_AND(long long a, long long int b) 
	{ 
	return (a & b);
	}

//' Pairwise AND operations
//'
//' @param a INTEGER to be manipulated
//' @param b INTEGER to be manipulated
//' @return Updated INTEGER after AND
// [[Rcpp::export]]
NumericVector bits_AND(const std::vector<long long int> arr, const std::vector<long long int> brr)
	{
	NumericVector r{};
	long long int i = 0;
	for (auto& element : arr) 
		{
		long long int res = b_AND(element, brr[i]);
		r.push_back(res);
		++i;
		}
	return r;
	}
	

	
long long b_OR(long long a, long long int b) 
	{ 
	return (a | b);
	}
	
	
//' Pairwise OR operations
//'
//' @param a INTEGER to be manipulated
//' @param b INTEGER to be manipulated
//' @return Updated INTEGER after OR
// [[Rcpp::export]]
NumericVector bits_OR(const std::vector<long long int> arr, const std::vector<long long int> brr)
	{
	NumericVector r{};
	long long int i = 0;
	for (auto& element : arr) 
		{
		long long int res = b_OR(element, brr[i]);
		r.push_back(res);
		++i;
		}
	return r;
	}	
	
long long b_XOR(long long a, long long int b) 
	{ 
	return (a ^ b);
	}


//' Pairwise XOR operations
//'
//' @param a INTEGER to be manipulated
//' @param b INTEGER to be manipulated
//' @return Updated INTEGER after XOR
// [[Rcpp::export]]
NumericVector bits_XOR(const std::vector<long long int> arr, const std::vector<long long int> brr)
	{
	NumericVector r{};
	long long int i = 0;
	for (auto& element : arr) 
		{
		long long int res = b_XOR(element, brr[i]);
		r.push_back(res);
		++i;
		}
	return r;
	}	


long long b_NOT(long long a) 
	{ 
	return (~a);
	}
	
//' NOT operation
//'
//' @param a INTEGER to be manipulated
//' @return Updated INTEGER after NOT
// [[Rcpp::export]]
NumericVector bits_NOT(const std::vector<long long int> arr)
	{
	NumericVector r{};
	for (auto& element : arr) 
		{
		long long int res = b_NOT(element);
		r.push_back(res);
		}
	return r;
	}	




	