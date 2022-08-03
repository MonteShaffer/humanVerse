#include <Rcpp/Lightest> // Rcpp 1.0.8 or newer

// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring




// [[Rcpp::export]]
inline std::string cpp_rtrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(s.find_last_not_of(t) + 1);
	return s;
	}

// [[Rcpp::export]]
inline std::string cpp_ltrim(std::string s, std::string t = " \t\n\r\f\v")
	{
	s.erase(0, s.find_first_not_of(t));
	return s;
	}

// [[Rcpp::export]]
inline std::string cpp_trim(std::string s, std::string t = " \t\n\r\f\v")
	{
	return cpp_ltrim(cpp_rtrim(s, t), t);
	}


// [[Rcpp::export]]
inline std::string cpp_tolower(std::string s)
	{
	std::for_each(s.begin(), s.end(), [](char & c)
	    {
        c = ::tolower(c);
        });
    return(s);
	}

// [[Rcpp::export]]
inline std::string cpp_toupper(std::string s)
	{
	std::for_each(s.begin(), s.end(), [](char & c)
	    {
        c = ::toupper(c);
        });
    return(s);
	}

// [[Rcpp::export]]
inline long long int cpp_strlen(std::string s)
	{
	return s.length();
	}

// [[Rcpp::export]]
inline char cpp_charAt(std::string s, long long unsigned int w)
	{
	// R indexes at [1], C++ indexes at [0]
	if(w > s.length()) return ('\0');
	if(w < 1 ) return ('\0');
	return s[w-1];
	}
	

// [[Rcpp::export]]
std::vector<std::string> cpp_explode(std::string s, std::string sep="")
	{
	std::vector<std::string> r{};
	if(sep == "")
	    {
	    std::string m;
	    for(long long unsigned int i = 0; i < s.length(); i++)
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

// [[Rcpp::export]]
std::string cpp_str_replace(std::string search, std::string replace, std::string subject)
	{
	std::vector<std::string> tmp = cpp_explode(subject, search);
	return cpp_implode(tmp, replace);
	}
	
	
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
	
