
// need for locale for string functions 
#include <unicode/unistr.h>
#include <unicode/ustream.h>
#include <unicode/locid.h>

#include <Rcpp.h>
using namespace Rcpp;








//' Repeat a String s
//'
//' @param String to 'repeat'
//' @param Integer 'times' to 'repeat'
//' @return updated String repeated 'times'	
// [[Rcpp::export]]
std::string s_str_repeat(std::string s, int times)
	{
	std::string out = "";
	if(times < 1) { return out; }
	for(int i = 0; i < times; i++)
			{
			out += s;
			}
	return out; 
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
		std::string res = s_str_repeat(element, times);
		r.push_back(res);
		}
	return r;
}














// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring


//' String Right Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, right trimmed
// [[Rcpp::export]]
std::string s_rtrim(std::string s, std::string t = " \t\n\r\f\v")
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
		std::string res = s_rtrim(element, t);
		r.push_back(res);
		}
	return r;
}

//' String Left Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, left trimmed
// [[Rcpp::export]]
std::string s_ltrim(std::string s, std::string t = " \t\n\r\f\v")
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
		std::string res = s_ltrim(element, t);
		r.push_back(res);
		}
	return r;
}


//' String Trim
//'
//' @param s String to be trimmed
//' @param t Trimming elements
//' @return Updated String s, trimmed (both left and right)
// [[Rcpp::export]]
std::string s_trim(std::string s, std::string t = " \t\n\r\f\v")
	{
	return s_ltrim(s_rtrim(s, t), t);
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
		std::string res = s_trim(element, t);
		r.push_back(res);
		}
	return r;
}


//' String to lower case
//'
//' @param s String to be transformed
//' @return Updated String s, lower cased
// [[Rcpp::export]]
std::string s_tolower(std::string s, std::string locale="en_US.UTF-8")
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
		std::string res = s_tolower(element, locale);
		r.push_back(res);
		}
	return r;
}

//' String to upper case
//'
//' @param s String to be transformed
//' @return Updated String s, upper cased
// [[Rcpp::export]]
std::string s_toupper(std::string s, std::string locale="en_US.UTF-8")
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
		std::string res = s_toupper(element, locale);
		r.push_back(res);
		}
	return r;
}

//' Get String Length
//'
//' @param s String to be sized
//' @return integer length
// [[Rcpp::export]]
long long unsigned int s_strlen(std::string s)
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
		long long unsigned int res = s_strlen(element);
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



//' Explode string into array based on separator
//'
//' @param sep String to determine how split
//' @param str String to be exploded (split)
//' @return vector of strings (List array as CharacterVector)
// [[Rcpp::export]]
std::vector<std::string> s_explode(std::string sep, std::string s)
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
		std::vector<std::string> res = s_explode(sep, element);
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
std::string s_implode(std::string sep, std::vector<std::string> r)
	{
	std::string s = "";
	int n = r.size();
	int i = 0;
	for (auto& element : r) 
			{
			s += element;
			++i; 
			if(i != n) { s += sep; } // no separator on last element
			}
	return s;
	}
	
//' Implode string array into string based on separator
//'
//' @param vector of strings (array as CharacterVector)
//' @param sep String to determine how join
//' @return joined String
// [[Rcpp::export]]
CharacterVector cpp_implode(std::string sep, Rcpp::List str)
{
	CharacterVector r{};
	for (auto& element : str) 
		{
		std::string res = s_implode(sep, element);
		r.push_back(res);
		}
	return r;
}	



//' Search/Replace a String Subject
//'
//' @param String to 'search'
//' @param String to 'replace'
//' @param String 'subject'
//' @return updated 'subject' String appropriate replaced ... (no REGEX here)
// [[Rcpp::export]]
std::string s_str_replace(const std::string search, const std::string replace, const std::string subject)
	{
	std::string res = subject;
	std::vector<std::string> tmp = s_explode(search, res);
	res = s_implode(replace, tmp);		
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
	
	std::string str;
	std::string mysearch;
	std::string myreplace;
	
	int slen = search.size();
	int rlen = replace.size();
	int nlen = subject.size();
	int si = 0;
	int ri = 0;
	int i;
	int j;
	int mlen = std::max(rlen, slen);
	
	// std::string res = s_str_replace(mysearch, myreplace, mysubject);
	// r.push_back(res);
	
	if(slen == rlen) /// pairwise over EACH subject
		{
		std::cout << " CASE 1 ";
		std::cout << "\n";
		for(j=0; j<nlen; j++)
			{
			str = subject[j];
			for(i=0; i<slen; i++)
				{
				mysearch = search[i];
				myreplace = replace[i];
				str = s_str_replace(mysearch, myreplace, str);
				}
			r.push_back(str);
			}
		return r;
		}
	
	if(rlen == 1)
		{
		std::cout << " CASE 2 ";
		std::cout << "\n";
		for(j=0; j<nlen; j++)
			{
			str = subject[j];
			for(i=0; i<slen; i++)
				{
				mysearch = search[i];
				myreplace = replace[0];
				str = s_str_replace(mysearch, myreplace, str);
				}
			r.push_back(str);
			}
		return r;
		}
		
	if(slen == 1 && rlen > nlen)
		{
		std::cout << " CASE 3 ";
		std::cout << "\n";
		si = 0;
		for(j=0; j<rlen; j++)
			{
			str = subject[si];
			mysearch = search[0];
			myreplace = replace[j];
			str = s_str_replace(mysearch, myreplace, str);
			r.push_back(str);
			si = 1 + si;
			if(si > nlen) { si = 0; }  // loop over s, end, back to beginning
			}
		return r;
		}
		
		std::cout << " CASE 4 ";
		std::cout << "\n";
		for(j=0; j<nlen; j++)
			{
			str = subject[j];
			si = 0;
			ri = 0;
			for(i=0; i<mlen; i++)
				{
				mysearch = search[si];
				myreplace = replace[ri];
				str = s_str_replace(mysearch, myreplace, str);
				
				// loop over s, end, back to beginning
				si = 1 + si;  if(si >= slen) { si = 0; }  
				// loop over s, end, back to beginning
				ri = 1 + ri;  if(ri >= rlen) { ri = 0; }  
				}				
			r.push_back(str);
			}
		return r;
		
			
}





// https://stackoverflow.com/questions/667183/padding-stl-strings-in-c
// no need to do str_pad
// https://stackoverflow.com/questions/26241085/rcpp-function-check-if-missing-value
// TODO ... how to implement missing values as INPUTS
// str.trim( c(1, NA), method="s"); str.trim( c(1, NA), method="c");

