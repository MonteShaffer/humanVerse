#include <Rcpp.h>
using namespace Rcpp;

//' Compute gcd / lcm simultaneously (Euler Method)
//'
//' @param x first integer
//' @param y second integer
//' @return two INTEGER values in list form (gcd) and (lcd)
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
		List L = List::create(Named("gcd") = a , Named("lcm") = (x / a * y ) );
	return L;
}

//' Compute gcd	(Euler Method)
//'
//' @param x first integer
//' @param y second integer
//' @return INTEGER (gcd)
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

//' Compute lcd	(Euler Method)
//'
//' @param x first integer
//' @param y second integer
//' @return INTEGER (lcd)
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
		return (x / a * y);
}
