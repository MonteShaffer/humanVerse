#include <Rcpp/Lightest> // Rcpp 1.0.8 or newer

// https://stackoverflow.com/questions/216823/how-to-trim-a-stdstring


// [[Rcpp::export]]
inline std::string& cpp_rtrim(std::string& s, const char* t = " \t\n\r\f\v")
{
    s.erase(s.find_last_not_of(t) + 1);
    return s;
}

// [[Rcpp::export]]
inline std::string& cpp_ltrim(std::string& s, const char* t = " \t\n\r\f\v")
{
    s.erase(0, s.find_first_not_of(t));
    return s;
}

// [[Rcpp::export]]
inline std::string& cpp_trim(std::string& s, const char* t = " \t\n\r\f\v")
{
    return cpp_ltrim(cpp_rtrim(s, t), t);
}




