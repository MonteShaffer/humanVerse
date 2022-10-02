#include <Rcpp.h>
#include <random>
#include <sstream>

using namespace Rcpp;
// https://stackoverflow.com/a/67891349/184614
// https://github.com/mariusbancila/stduuid
// https://stackoverflow.com/a/58467162/184614
// Version 1 concatenates the 48-bit MAC address of the "node" (that is, the computer generating the UUID), with a 60-bit timestamp, being the number of 100-nanosecond intervals since midnight 15 October 1582 Coordinated Universal Time (UTC), the date on which the Gregorian calendar was first adopted. RFC 4122 states that the time v

std::string get_uuid() 
	{
	static std::random_device dev;
	static std::mt19937 rng(dev());

	std::uniform_int_distribution<int> dist(0, 15);

	const char *v = "0123456789abcdef";
	const bool dash[] = { 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0 };

	std::string res;
	for (int i = 0; i < 16; i++) 
			{
			if (dash[i]) { res += "-"; }
			res += v[dist(rng)];
			res += v[dist(rng)];
			}
	return res;
} 

// [[Rcpp::export]]
std::string s_uuid_basic()
	{
	std::string res = get_uuid();
	return res; 
	}



// [[Rcpp::export]]
CharacterVector cpp_uuid_basic(int n=5)
{
	CharacterVector r{};
	int i;
	for(i = 0; i < n; i++)
		{
		std::string res = get_uuid();
		r.push_back(res);
		}
	return r;
}



std::string generate_uuid_v4() 
	{
	static std::random_device	rd;
	static std::mt19937_64		gen(rd());
	static std::uniform_int_distribution<> D(0, 15);
	static std::uniform_int_distribution<> D2(8, 11);

	std::stringstream ss;
	int i;
	ss << std::hex;
	for (i = 0; i < 8; i++) 
		{
		ss << D(gen);
		}
	ss << "-";
	for (i = 0; i < 4; i++) 
		{
		ss << D(gen);
		}
	ss << "-4";
	for (i = 0; i < 3; i++) 
		{
		ss << D(gen);
		}
	ss << "-";
	ss << D2(gen);
	for (i = 0; i < 3; i++) 
		{
		ss << D(gen);
		}
	ss << "-";
	for (i = 0; i < 12; i++) 
		{
		ss << D(gen);
		}
	return ss.str();
	}



// [[Rcpp::export]]
std::string s_uuid_basic_v4()
	{
	std::string res = generate_uuid_v4();
	return res; 
	}


// [[Rcpp::export]]
CharacterVector cpp_uuid_basic_v4(int n=5)
{
	CharacterVector r{};
	int i;
	for(i = 0; i < n; i++)
		{
		std::string res = generate_uuid_v4();
		r.push_back(res);
		}
	return r;
}