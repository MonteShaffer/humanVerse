#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <iostream>
#include <string>

using namespace Rcpp;


//' Convert an integer to a string of base 
//'
//' @param num INTEGER to be converted
//' @param base INTEGER of base (2,32)
//' @return string of integer in the new base 
// [[Rcpp::export]]
std::string s_int2base(long long int num, int base=16)
	{
	std::string d = "0123456789ABCDEFGHIJKLMNOPQRSTUV";
	std::string res;
	if(num == 0) { return "0"; }
	while(num > 0)
		{
		res = d[num % base] + res;
		num /= base;
		}
	return res;
	}
	
//' Convert an integer to a string of base 
//'
//' @param num INTEGER to be converted
//' @param base INTEGER of base (2,32)
//' @return string of integer in the new base 
// [[Rcpp::export]]
CharacterVector cpp_int2base(const std::vector<long long int> num, int base=16)
{
	CharacterVector r{};
	for (auto& element : num) 
		{
		std::string res = s_int2base(element, base);
		r.push_back(res);
		}
	return r;
}

	
//' Convert an string of base back to BASE10 INTEGER
//'
//' @param num INTEGER to be converted
//' @param base INTEGER of base (2,32)
//' @return string of integer in the new base 
// [[Rcpp::export]]
long long int s_base2int(std::string s, int base=16)
	{	
	long long int res = std::stoi( s, 0, base );
	return res;
	}
	

//' Convert an string of base back to BASE10 INTEGER
//'
//' @param num INTEGER to be converted
//' @param base INTEGER of base (2,32)
//' @return string of integer in the new base 
// [[Rcpp::export]]	
NumericVector cpp_base2int(const std::vector<std::string> str, int base=16)
	{
	NumericVector r{};
	for (auto& element : str) 
		{
		long long int res = s_base2int(element, base);
		r.push_back(res);
		}
	return r;	
	}
	
	
	

//' Convert an integer to a string of base 
//'
//' @param num INTEGER to be converted
//' @param base INTEGER of base (2,32)
//' @return string of integer in the new base 

// https://codescracker.com/cpp/program/cpp-program-convert-octal-to-binary.htm
// [[Rcpp::export]]
std::string s_base2base(std::string s, int from=16, int to=2)
	{
	// from (2,32) ... to (2,32)
	std::string d = "0123456789ABCDEFGHIJKLMNOPQRSTUV";
	
	std::string res;
	
	// do something here ... 
	
	return res;
	}


// [[Rcpp::export]]
CharacterVector cpp_base2base(const std::vector<std::string> str, int from=16, int to=2)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_base2base(element, from, to);
		r.push_back(res);
		}
	return r;
}