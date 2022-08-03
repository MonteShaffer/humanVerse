
#include <Rcpp.h>
using namespace Rcpp;

// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring



//' String Right Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, right trimmed
// [[Rcpp::export]]
std::string cpp_rtrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(s.find_last_not_of(t) + 1);
	return s;
	}

//' String Left Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, left trimmed
// [[Rcpp::export]]
std::string cpp_ltrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(0, s.find_first_not_of(t));
	return s;
	}

//' String Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, trimmed (both left and right)
// [[Rcpp::export]]
std::string cpp_trim(std::string s, std::string t = " \t\n\r\f\v")
	{
	return cpp_ltrim(cpp_rtrim(s, t), t);
	}


//' String to lower case
//'
//' @param s String to be transformed
//' @return Updated String s, lower cased
// [[Rcpp::export]]
std::string cpp_tolower(std::string s)
	{
	std::for_each(s.begin(), s.end(), [](char & c)
	    {
        c = ::tolower(c);
        });
    return(s);
	}

//' String to upper case
//'
//' @param s String to be transformed
//' @return Updated String s, upper cased
// [[Rcpp::export]]
std::string cpp_toupper(std::string s)
	{
	std::for_each(s.begin(), s.end(), [](char & c)
	    {
        c = ::toupper(c);
        });
    return(s);
	}

//' Get String Length
//'
//' @param s String to be sized
//' @return integer length
// [[Rcpp::export]]
long long unsigned int cpp_strlen(std::string s)
	{
	return s.length();
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

//' Explode string into array based on separator
//'
//' @param s String to be exploded (split)
//' @param sep String to determine how split
//' @return vector of strings (array as CharacterVector)
std::vector<std::string> cpp_explode(std::string s, std::string sep="")
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
	


//' Implode string array into string based on separator
//'
//' @param vector of strings (array as CharacterVector)
//' @param sep String to determine how join
//' @return joined String
// [[Rcpp::export]]
std::string cpp_implode(std::vector<std::string> r, std::string sep="")
	{
	std::string s = "";
	for (auto& element : r) 
	    {
        s += element;
        s += sep;
        }
        s = cpp_rtrim(s, sep);
    return(s);
	}

//' Search/Replace a String Subject
//'
//' @param String to 'search'
//' @param String to 'replace'
//' @param String 'subject'
//' @return updated 'subject' String appropriate replaced ... (no REGEX here)
// [[Rcpp::export]]
std::string cpp_str_replace(std::string search, std::string replace, std::string subject)
	{
	std::vector<std::string> tmp = cpp_explode(subject, search);
	return cpp_implode(tmp, replace);
	}
	

//' Repeat a String s
//'
//' @param String to 'repeat'
//' @param Integer 'times' to 'repeat'
//' @return updated String repeated 'times'	
// [[Rcpp::export]]
std::string cpp_str_repeat(std::string s, int times)
	{
	std::string out = "";
	for(int i = 0; i < times; i++)
	    {
    	out += s;
	    }
    return(out); 
	}
	
