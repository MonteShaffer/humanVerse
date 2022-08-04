#include <unicode/unistr.h>
#include <unicode/ustream.h>
#include <unicode/locid.h>

#include <Rcpp.h>
using namespace Rcpp;

// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring



std::string _rtrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(s.find_last_not_of(t) + 1);
	return s;
	}
	
//' String Right Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, right trimmed
// [[Rcpp::export]]
CharacterVector cpp_rtrim(const std::vector<std::string> str, std::string t = " \t\n\r\f\v")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = _rtrim(element, t);
		r.push_back(res);
		}
	return r;
}


std::string _ltrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(0, s.find_first_not_of(t));
	return s;
	}

//' String Left Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, left trimmed
// [[Rcpp::export]]
CharacterVector cpp_ltrim(const std::vector<std::string> str, std::string t = " \t\n\r\f\v")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = _ltrim(element, t);
		r.push_back(res);
		}
	return r;
}


std::string _trim(std::string s, std::string t = " \t\n\r\f\v")
	{
	return _ltrim(_rtrim(s, t), t);
	}
	
//' String Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, trimmed (both left and right)
// [[Rcpp::export]]
CharacterVector cpp_trim(const std::vector<std::string> str, std::string t = " \t\n\r\f\v")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = _trim(element, t);
		r.push_back(res);
		}
	return r;
}


std::string cpp_tolower(std::string s, std::string locale="en_US.UTF-8")
	{
	// maybe locale.in ... locale.out 
	// NONTRIVIAL
	// https://stackoverflow.com/questions/313970/how-to-convert-an-instance-of-stdstring-to-lower-case
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
	/*
	// boost::to_upper was a bottle-neck ... DNA (not international)
	// https://stackoverflow.com/questions/313970/how-to-convert-an-instance-of-stdstring-to-lower-case
	// https://stackoverflow.com/questions/10688831/fastest-way-to-capitalize-words
	std::transform(s.begin(), s.end(), s.begin(), ::tolower);
	return s;
	*/
	/*
	// https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/
	std::for_each(s.begin(), s.end(), [](char & c)
			{
				c = ::tolower(c);
				});
		return s ;
	*/
	}

//' String to lower case
//'
//' @param s String to be transformed
//' @return Updated String s, lower cased
// [[Rcpp::export]]
CharacterVector cpp_strtolower(const std::vector<std::string> str, std::string locale="en_US.UTF-8")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = cpp_tolower(element, locale);
		r.push_back(res);
		}
	return r;
}

std::string cpp_toupper(std::string s, std::string locale="en_US.UTF-8")
	{
	std::transform(s.begin(), s.end(), s.begin(), ::toupper);
	return s;
	/*
	// https://www.geeksforgeeks.org/conversion-whole-string-uppercase-lowercase-using-stl-c/
	std::for_each(s.begin(), s.end(), [](char & c)
			{
				c = ::toupper(c);
				});
		return s;
	*/
	}
	
//' String to upper case
//'
//' @param s String to be transformed
//' @return Updated String s, upper cased
// [[Rcpp::export]]
CharacterVector cpp_strtoupper(const std::vector<std::string> str, std::string locale="en_US.UTF-8")
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = cpp_toupper(element, locale);
		r.push_back(res);
		}
	return r;
}


long long unsigned int _strlen(std::string s)
	{
	return s.length();
	}
//' Get String Length
//'
//' @param s String to be sized
//' @return integer length
// [[Rcpp::export]]
NumericVector cpp_strlen(const std::vector<std::string> str)
{
	NumericVector r{};
	for (auto& element : str) 
		{
		long long unsigned int res = _strlen(element);
		r.push_back(res);
		}
	return r;
}


/*
// [[Rcpp::export]]
inline char cpp_charAt(std::string s, long long int w)
	{
	// R indexes at [1], C++ indexes at [0]
	if(w > s.length()) return ('\0');
	if(w < 1 ) return ('\0');
	return s[w-1];
	}
*/



std::vector<std::string> _explode(std::string sep, std::string s)
	{
	std::vector<std::string> r{};
	if(sep == "")
			{
			std::string m;
			for(long long int unsigned i = 0; i < s.length(); i++)
					{
					m = s[i];
					r.push_back( m );				 
					}
			return r;	 
			}

	size_t pos = 0;
		std::string token;
		while ((pos = s.find(sep)) != std::string::npos) 
				{
				token = s.substr(0, pos);
				r.push_back(token);
				s.erase(0, pos + sep.length());
				}
		if(s.size() > 0) { r.push_back(s);}
	return r;
	}


//' Explode string into array based on separator
//'
//' @param sep String to determine how split
//' @param str String to be exploded (split)
//' @return vector of strings (List array as CharacterVector)
// [[Rcpp::export]]
List cpp_explode(std::string sep, const std::vector<std::string> str)
{
	List r{};
	for (auto& element : str) 
		{
		std::vector<std::string> res = _explode(sep, element);
		r.push_back(res);
		}
	return r;
}	


//' Implode string array into string based on separator
//'
//' @param vector of strings (array as CharacterVector)
//' @param sep String to determine how join
//' @return joined String
// [[Rcpp::export]]
CharacterVector cpp_implode(std::string sep, List<std::string> str)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::vector<std::string> res = _implode(sep, element);
		r.push_back(res);
		}
	return r;
}	

std::string _implode(std::string sep, std::vector<std::string> r)
	{
	std::string s = "";
	for (auto& element : r) 
			{
				s += element;
				s += sep;
				}
				s = _rtrim(s, sep);
		return(s);
	}


std::string _str_replace(const std::vector<std::string> search, const std::vector<std::string> replace, std::string subject)
	{
	long long unsigned int slen = search.size();
	long long unsigned int rlen = replace.size();
	long long unsigned int mlen = std::max(slen, rlen);
	std::string res = subject;
	
	for(long long unsigned int i = 0; i < mlen; i++)
		{
		std::string mysearch = (slen == 1) ? search[0] : search[i];
		std::string myreplace = (rlen == 1) ? replace[0] : replace[i];
		std::vector<std::string> tmp = cpp_explode(mysearch, res);
		std::string res = cpp_implode(myreplace, tmp);
		}
	
	return res;
	}
	
//' Search/Replace a String Subject
//'
//' @param String to 'search'
//' @param String to 'replace'
//' @param String 'subject'
//' @return updated 'subject' String appropriate replaced ... (no REGEX here)
// [[Rcpp::export]]
CharacterVector cpp_str_replace(const std::vector<std::string> search, const std::vector<std::string> replace, const std::vector<std::string> subject)
{
	CharacterVector r{};
	for (auto& element : subject) 
		{
		std::string res = _str_replace(search, replace, element);
		r.push_back(res);
		}
	return r;
}


std::string _str_repeat(std::string s, int times)
	{
	std::string out = "";
	for(int i = 0; i < times; i++)
			{
			out += s;
			}
		return(out); 
	}
	

//' Repeat a String s
//'
//' @param String to 'repeat'
//' @param Integer 'times' to 'repeat'
//' @return updated String repeated 'times'	
// [[Rcpp::export]]
CharacterVector cpp_str_repeat(const std::vector<std::string> str, int times)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = _str_repeat(element, times);
		r.push_back(res);
		}
	return r;
}

