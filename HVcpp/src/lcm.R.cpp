#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List cpp_gcd_lcm(long long int x, long long int y) {
	long long int a = x;
	long long int b = y;
	long long int t = b;
	while(b != 0)
		{
		t = b;
		b = a % b;
		a = t;
		}		
    List L = List::create(Named("gcd") = a , Named("lcm") = (x * y ) / a );
	return L;
}

// [[Rcpp::export]]
long long int cpp_gcd(long long int x, long long int y) {
	long long int a = x;
	long long int b = y;
	long long int t = b;
	while(b != 0)
		{
		t = b;
		b = a % b;
		a = t;
		}
    return a;
}

// [[Rcpp::export]]
long long int cpp_lcm(long long int x, long long int y) {
    long long int a = x;
	long long int b = y;
	long long int t = b;
	while(b != 0)
		{
		t = b;
		b = a % b;
		a = t;
		}
    return (x*y)/a;
}
