#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <iostream>
#include <string>

using namespace Rcpp;


//' Compute NumericVector of Primes
//'
//' (ported from pracma::primes) with new 'n' primes functionality
//' SLOWS above 10^7
//'
//' @param n How Many
//' @param first BOOLEAN (if FALSE) returns primes <= 'n' ELSE returns 'n' primes
//' @return NumberVector of PRIMES 
// [[Rcpp::export]]
NumericVector cpp_primes(long long int n, bool first=false) {
	long long gn = n;
		if(first) { gn = ceil( n * log(n) + n * log(log(n)) ); }
		long long gn_sqrt = floor( sqrt((double) gn) );				 
		std::vector<long long int> p;
		
		long int i;
		for(i = 1; i < (1+gn); i+=2)
				{
				p.push_back(i);
				}
		p[0] = 2;
		
		long long int q;	
	q = p.size();
		
		if(gn >= 9)	// we already have primes except for 9 ... 
				{
				long long int k;
						long long int k_idx;
						long long int k2_idx;
				long long int j;
				for(k =3; k < (1+gn_sqrt); k+=2)
						{
						k_idx = (k+1)/2 - 1;
						if (p[k_idx] != 0)
				{
				k2_idx = (k * k + 1)/2 - 1;	
				for(j = k2_idx; j <= (q); j+=k)
						{
						p[j] = 0;
						}
				}
						}
				}
		
		// p = p[p > 0];
		p.erase( remove(p.begin(), p.end(), 0), p.end());		
		p.shrink_to_fit();
				
	q = p.size();	
		if(q > n)
				{
				p.erase( p.begin()+n, p.end() );
				p.shrink_to_fit();
				}
		
	
	/*
	https://www.quora.com/Which-is-the-fastest-algorithm-to-find-prime-numbers-using-C
	https://onlinegdb.com/9OZ2BkZUY ... // this is pracma::prime algorithm adapted to allow "first" N 
	*/
	// https://stackoverflow.com/questions/40446846/c-array-to-rcpp-numericvector
	// return NumericVector(p,p+sizeof(p)/sizeof(*p));
	
	return	wrap(p);
}



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