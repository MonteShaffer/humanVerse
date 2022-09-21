#include <Rcpp.h>
using namespace Rcpp;

// https://codereview.stackexchange.com/a/132885/120274
/* nano
hacked to Rcpp by Monte J. Shaffer (monte.shaffer@gmail.com)
	(VECTORIZED, TIMES) [9/2022] 
	LICENSE:	MIT extension of below.
*/
	
#ifndef CURRENT_TIME_H
#define CURRENT_TIME_H

#include <chrono>
#include <cstdint>

class CurrentTime {
    std::chrono::high_resolution_clock m_clock;

public:
    uint64_t milliseconds();
    uint64_t microseconds();
    uint64_t nanoseconds();
};

uint64_t CurrentTime::milliseconds() 
{
    return std::chrono::duration_cast<std::chrono::milliseconds>
              (m_clock.now().time_since_epoch()).count();
}

uint64_t CurrentTime::microseconds() 
{
    return std::chrono::duration_cast<std::chrono::microseconds>
              (m_clock.now().time_since_epoch()).count();
}

uint64_t CurrentTime::nanoseconds()
{
    return std::chrono::duration_cast<std::chrono::nanoseconds>
              (m_clock.now().time_since_epoch()).count();
}    

#endif  /* CURRENT_TIME_H */


//////////////////////////////

//' Get System nanoseconds
//'
//' @return time in nanosecond
// [[Rcpp::export]]
uint64_t cpp_nano()
{
	std::chrono::high_resolution_clock m_clock;
	return std::chrono::duration_cast<std::chrono::nanoseconds>
		(m_clock.now().time_since_epoch()).count();
}

// NumericVector?
//' Get System nanoseconds
//'
//' @return time in nanosecond
// [[Rcpp::export]]
uint64_t cpp_micro()
{
	std::chrono::high_resolution_clock m_clock;
	return std::chrono::duration_cast<std::chrono::microseconds>
		(m_clock.now().time_since_epoch()).count();
}


//' Get System nanoseconds
//'
//' @return time in nanosecond
// [[Rcpp::export]]
uint64_t cpp_milli()
{
	std::chrono::high_resolution_clock m_clock;
	return std::chrono::duration_cast<std::chrono::milliseconds>
		(m_clock.now().time_since_epoch()).count();
}


//' Get System now based on precision
//'
//' @return time in input 
// [[Rcpp::export]]
uint64_t cpp_now(std::string precision="nano")
{
	if(precision == "milli") 	{ return cpp_milli(); }
	if(precision == "micro") { return cpp_micro(); }
	// default
	return cpp_nano();
}


//' Get System time (Similar to Sys.time())
//'
//' @return time in input 
// [[Rcpp::export]]
double cpp_time()
{
	return cpp_nano()/1000000000;
}