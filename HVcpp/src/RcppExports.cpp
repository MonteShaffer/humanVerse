// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// bits_RShift
NumericVector bits_RShift(const std::vector<long long int> arr, int b);
RcppExport SEXP _HVcpp_bits_RShift(SEXP arrSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(bits_RShift(arr, b));
    return rcpp_result_gen;
END_RCPP
}
// bits_LShift
NumericVector bits_LShift(const std::vector<long long int> arr, int b);
RcppExport SEXP _HVcpp_bits_LShift(SEXP arrSEXP, SEXP bSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< int >::type b(bSEXP);
    rcpp_result_gen = Rcpp::wrap(bits_LShift(arr, b));
    return rcpp_result_gen;
END_RCPP
}
// bits_AND
NumericVector bits_AND(const std::vector<long long int> arr, const std::vector<long long int> brr);
RcppExport SEXP _HVcpp_bits_AND(SEXP arrSEXP, SEXP brrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type brr(brrSEXP);
    rcpp_result_gen = Rcpp::wrap(bits_AND(arr, brr));
    return rcpp_result_gen;
END_RCPP
}
// bits_OR
NumericVector bits_OR(const std::vector<long long int> arr, const std::vector<long long int> brr);
RcppExport SEXP _HVcpp_bits_OR(SEXP arrSEXP, SEXP brrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type brr(brrSEXP);
    rcpp_result_gen = Rcpp::wrap(bits_OR(arr, brr));
    return rcpp_result_gen;
END_RCPP
}
// bits_XOR
NumericVector bits_XOR(const std::vector<long long int> arr, const std::vector<long long int> brr);
RcppExport SEXP _HVcpp_bits_XOR(SEXP arrSEXP, SEXP brrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type brr(brrSEXP);
    rcpp_result_gen = Rcpp::wrap(bits_XOR(arr, brr));
    return rcpp_result_gen;
END_RCPP
}
// bits_NOT
NumericVector bits_NOT(const std::vector<long long int> arr);
RcppExport SEXP _HVcpp_bits_NOT(SEXP arrSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<long long int> >::type arr(arrSEXP);
    rcpp_result_gen = Rcpp::wrap(bits_NOT(arr));
    return rcpp_result_gen;
END_RCPP
}
// cpp_gcd_lcm
List cpp_gcd_lcm(long long int x, long long int y);
RcppExport SEXP _HVcpp_cpp_gcd_lcm(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< long long int >::type x(xSEXP);
    Rcpp::traits::input_parameter< long long int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_gcd_lcm(x, y));
    return rcpp_result_gen;
END_RCPP
}
// cpp_gcd
long long int cpp_gcd(long long int x, long long int y);
RcppExport SEXP _HVcpp_cpp_gcd(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< long long int >::type x(xSEXP);
    Rcpp::traits::input_parameter< long long int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_gcd(x, y));
    return rcpp_result_gen;
END_RCPP
}
// cpp_lcm
long long int cpp_lcm(long long int x, long long int y);
RcppExport SEXP _HVcpp_cpp_lcm(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< long long int >::type x(xSEXP);
    Rcpp::traits::input_parameter< long long int >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_lcm(x, y));
    return rcpp_result_gen;
END_RCPP
}
// matrix_diagonal
SEXP matrix_diagonal(Eigen::MatrixXd A);
RcppExport SEXP _HVcpp_matrix_diagonal(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_diagonal(A));
    return rcpp_result_gen;
END_RCPP
}
// matrix_rank
SEXP matrix_rank(Eigen::MatrixXd A);
RcppExport SEXP _HVcpp_matrix_rank(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_rank(A));
    return rcpp_result_gen;
END_RCPP
}
// matrix_transpose
SEXP matrix_transpose(Eigen::MatrixXd A);
RcppExport SEXP _HVcpp_matrix_transpose(SEXP ASEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type A(ASEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_transpose(A));
    return rcpp_result_gen;
END_RCPP
}
// matrix_multiplication
SEXP matrix_multiplication(Eigen::MatrixXd A, Eigen::MatrixXd B);
RcppExport SEXP _HVcpp_matrix_multiplication(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::MatrixXd >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_multiplication(A, B));
    return rcpp_result_gen;
END_RCPP
}
// matrix_multiplication_map
SEXP matrix_multiplication_map(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B);
RcppExport SEXP _HVcpp_matrix_multiplication_map(SEXP ASEXP, SEXP BSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::Map<Eigen::MatrixXd> >::type A(ASEXP);
    Rcpp::traits::input_parameter< Eigen::Map<Eigen::MatrixXd> >::type B(BSEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_multiplication_map(A, B));
    return rcpp_result_gen;
END_RCPP
}
// matrix_rank_real
unsigned matrix_rank_real(const Eigen::MatrixXd& M);
RcppExport SEXP _HVcpp_matrix_rank_real(SEXP MSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type M(MSEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_rank_real(M));
    return rcpp_result_gen;
END_RCPP
}
// matrix_rank_complex
unsigned matrix_rank_complex(const Eigen::MatrixXd& Re, const Eigen::MatrixXd& Im);
RcppExport SEXP _HVcpp_matrix_rank_complex(SEXP ReSEXP, SEXP ImSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type Re(ReSEXP);
    Rcpp::traits::input_parameter< const Eigen::MatrixXd& >::type Im(ImSEXP);
    rcpp_result_gen = Rcpp::wrap(matrix_rank_complex(Re, Im));
    return rcpp_result_gen;
END_RCPP
}
// cpp_md5
CharacterVector cpp_md5(const std::vector<std::string> str, int times);
RcppExport SEXP _HVcpp_cpp_md5(SEXP strSEXP, SEXP timesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< int >::type times(timesSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_md5(str, times));
    return rcpp_result_gen;
END_RCPP
}
// cpp_primes
NumericVector cpp_primes(long long int n, bool first);
RcppExport SEXP _HVcpp_cpp_primes(SEXP nSEXP, SEXP firstSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< long long int >::type n(nSEXP);
    Rcpp::traits::input_parameter< bool >::type first(firstSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_primes(n, first));
    return rcpp_result_gen;
END_RCPP
}
// cpp_sort_numeric_works
NumericVector cpp_sort_numeric_works(NumericVector arr, std::string dir);
RcppExport SEXP _HVcpp_cpp_sort_numeric_works(SEXP arrSEXP, SEXP dirSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< std::string >::type dir(dirSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_sort_numeric_works(arr, dir));
    return rcpp_result_gen;
END_RCPP
}
// cpp_sort_numeric
NumericVector cpp_sort_numeric(NumericVector arr, NumericVector partial, std::string dir);
RcppExport SEXP _HVcpp_cpp_sort_numeric(SEXP arrSEXP, SEXP partialSEXP, SEXP dirSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type arr(arrSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type partial(partialSEXP);
    Rcpp::traits::input_parameter< std::string >::type dir(dirSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_sort_numeric(arr, partial, dir));
    return rcpp_result_gen;
END_RCPP
}
// s_str_repeat
std::string s_str_repeat(std::string s, int times);
RcppExport SEXP _HVcpp_s_str_repeat(SEXP sSEXP, SEXP timesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< int >::type times(timesSEXP);
    rcpp_result_gen = Rcpp::wrap(s_str_repeat(s, times));
    return rcpp_result_gen;
END_RCPP
}
// cpp_str_repeat
CharacterVector cpp_str_repeat(const std::vector<std::string> str, int times);
RcppExport SEXP _HVcpp_cpp_str_repeat(SEXP strSEXP, SEXP timesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< int >::type times(timesSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_str_repeat(str, times));
    return rcpp_result_gen;
END_RCPP
}
// s_rtrim
std::string s_rtrim(std::string s, std::string t);
RcppExport SEXP _HVcpp_s_rtrim(SEXP sSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< std::string >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(s_rtrim(s, t));
    return rcpp_result_gen;
END_RCPP
}
// cpp_rtrim
CharacterVector cpp_rtrim(const std::vector<std::string> str, std::string t);
RcppExport SEXP _HVcpp_cpp_rtrim(SEXP strSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_rtrim(str, t));
    return rcpp_result_gen;
END_RCPP
}
// s_ltrim
std::string s_ltrim(std::string s, std::string t);
RcppExport SEXP _HVcpp_s_ltrim(SEXP sSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< std::string >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(s_ltrim(s, t));
    return rcpp_result_gen;
END_RCPP
}
// cpp_ltrim
CharacterVector cpp_ltrim(const std::vector<std::string> str, std::string t);
RcppExport SEXP _HVcpp_cpp_ltrim(SEXP strSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_ltrim(str, t));
    return rcpp_result_gen;
END_RCPP
}
// s_trim
std::string s_trim(std::string s, std::string t);
RcppExport SEXP _HVcpp_s_trim(SEXP sSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< std::string >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(s_trim(s, t));
    return rcpp_result_gen;
END_RCPP
}
// cpp_trim
CharacterVector cpp_trim(const std::vector<std::string> str, std::string t);
RcppExport SEXP _HVcpp_cpp_trim(SEXP strSEXP, SEXP tSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type t(tSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_trim(str, t));
    return rcpp_result_gen;
END_RCPP
}
// s_tolower
std::string s_tolower(std::string s, std::string locale);
RcppExport SEXP _HVcpp_s_tolower(SEXP sSEXP, SEXP localeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< std::string >::type locale(localeSEXP);
    rcpp_result_gen = Rcpp::wrap(s_tolower(s, locale));
    return rcpp_result_gen;
END_RCPP
}
// cpp_strtolower
CharacterVector cpp_strtolower(const std::vector<std::string> str, std::string locale);
RcppExport SEXP _HVcpp_cpp_strtolower(SEXP strSEXP, SEXP localeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type locale(localeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_strtolower(str, locale));
    return rcpp_result_gen;
END_RCPP
}
// s_toupper
std::string s_toupper(std::string s, std::string locale);
RcppExport SEXP _HVcpp_s_toupper(SEXP sSEXP, SEXP localeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    Rcpp::traits::input_parameter< std::string >::type locale(localeSEXP);
    rcpp_result_gen = Rcpp::wrap(s_toupper(s, locale));
    return rcpp_result_gen;
END_RCPP
}
// cpp_strtoupper
CharacterVector cpp_strtoupper(const std::vector<std::string> str, std::string locale);
RcppExport SEXP _HVcpp_cpp_strtoupper(SEXP strSEXP, SEXP localeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    Rcpp::traits::input_parameter< std::string >::type locale(localeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_strtoupper(str, locale));
    return rcpp_result_gen;
END_RCPP
}
// s_strlen
long long unsigned int s_strlen(std::string s);
RcppExport SEXP _HVcpp_s_strlen(SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(s_strlen(s));
    return rcpp_result_gen;
END_RCPP
}
// cpp_strlen
NumericVector cpp_strlen(const std::vector<std::string> str);
RcppExport SEXP _HVcpp_cpp_strlen(SEXP strSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_strlen(str));
    return rcpp_result_gen;
END_RCPP
}
// s_explode
std::vector<std::string> s_explode(std::string sep, std::string s);
RcppExport SEXP _HVcpp_s_explode(SEXP sepSEXP, SEXP sSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< std::string >::type s(sSEXP);
    rcpp_result_gen = Rcpp::wrap(s_explode(sep, s));
    return rcpp_result_gen;
END_RCPP
}
// cpp_explode
List cpp_explode(std::string sep, const std::vector<std::string> str);
RcppExport SEXP _HVcpp_cpp_explode(SEXP sepSEXP, SEXP strSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type str(strSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_explode(sep, str));
    return rcpp_result_gen;
END_RCPP
}
// s_implode
std::string s_implode(std::string sep, std::vector<std::string> r);
RcppExport SEXP _HVcpp_s_implode(SEXP sepSEXP, SEXP rSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< std::vector<std::string> >::type r(rSEXP);
    rcpp_result_gen = Rcpp::wrap(s_implode(sep, r));
    return rcpp_result_gen;
END_RCPP
}
// cpp_implode
CharacterVector cpp_implode(std::string sep, Rcpp::List str);
RcppExport SEXP _HVcpp_cpp_implode(SEXP sepSEXP, SEXP strSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< std::string >::type sep(sepSEXP);
    Rcpp::traits::input_parameter< Rcpp::List >::type str(strSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_implode(sep, str));
    return rcpp_result_gen;
END_RCPP
}
// s_str_replace
std::string s_str_replace(const std::string search, const std::string replace, const std::string subject);
RcppExport SEXP _HVcpp_s_str_replace(SEXP searchSEXP, SEXP replaceSEXP, SEXP subjectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::string >::type search(searchSEXP);
    Rcpp::traits::input_parameter< const std::string >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< const std::string >::type subject(subjectSEXP);
    rcpp_result_gen = Rcpp::wrap(s_str_replace(search, replace, subject));
    return rcpp_result_gen;
END_RCPP
}
// cpp_str_replace
CharacterVector cpp_str_replace(const std::vector<std::string> search, const std::vector<std::string> replace, const std::vector<std::string> subject);
RcppExport SEXP _HVcpp_cpp_str_replace(SEXP searchSEXP, SEXP replaceSEXP, SEXP subjectSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type search(searchSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type replace(replaceSEXP);
    Rcpp::traits::input_parameter< const std::vector<std::string> >::type subject(subjectSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_str_replace(search, replace, subject));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_HVcpp_bits_RShift", (DL_FUNC) &_HVcpp_bits_RShift, 2},
    {"_HVcpp_bits_LShift", (DL_FUNC) &_HVcpp_bits_LShift, 2},
    {"_HVcpp_bits_AND", (DL_FUNC) &_HVcpp_bits_AND, 2},
    {"_HVcpp_bits_OR", (DL_FUNC) &_HVcpp_bits_OR, 2},
    {"_HVcpp_bits_XOR", (DL_FUNC) &_HVcpp_bits_XOR, 2},
    {"_HVcpp_bits_NOT", (DL_FUNC) &_HVcpp_bits_NOT, 1},
    {"_HVcpp_cpp_gcd_lcm", (DL_FUNC) &_HVcpp_cpp_gcd_lcm, 2},
    {"_HVcpp_cpp_gcd", (DL_FUNC) &_HVcpp_cpp_gcd, 2},
    {"_HVcpp_cpp_lcm", (DL_FUNC) &_HVcpp_cpp_lcm, 2},
    {"_HVcpp_matrix_diagonal", (DL_FUNC) &_HVcpp_matrix_diagonal, 1},
    {"_HVcpp_matrix_rank", (DL_FUNC) &_HVcpp_matrix_rank, 1},
    {"_HVcpp_matrix_transpose", (DL_FUNC) &_HVcpp_matrix_transpose, 1},
    {"_HVcpp_matrix_multiplication", (DL_FUNC) &_HVcpp_matrix_multiplication, 2},
    {"_HVcpp_matrix_multiplication_map", (DL_FUNC) &_HVcpp_matrix_multiplication_map, 2},
    {"_HVcpp_matrix_rank_real", (DL_FUNC) &_HVcpp_matrix_rank_real, 1},
    {"_HVcpp_matrix_rank_complex", (DL_FUNC) &_HVcpp_matrix_rank_complex, 2},
    {"_HVcpp_cpp_md5", (DL_FUNC) &_HVcpp_cpp_md5, 2},
    {"_HVcpp_cpp_primes", (DL_FUNC) &_HVcpp_cpp_primes, 2},
    {"_HVcpp_cpp_sort_numeric_works", (DL_FUNC) &_HVcpp_cpp_sort_numeric_works, 2},
    {"_HVcpp_cpp_sort_numeric", (DL_FUNC) &_HVcpp_cpp_sort_numeric, 3},
    {"_HVcpp_s_str_repeat", (DL_FUNC) &_HVcpp_s_str_repeat, 2},
    {"_HVcpp_cpp_str_repeat", (DL_FUNC) &_HVcpp_cpp_str_repeat, 2},
    {"_HVcpp_s_rtrim", (DL_FUNC) &_HVcpp_s_rtrim, 2},
    {"_HVcpp_cpp_rtrim", (DL_FUNC) &_HVcpp_cpp_rtrim, 2},
    {"_HVcpp_s_ltrim", (DL_FUNC) &_HVcpp_s_ltrim, 2},
    {"_HVcpp_cpp_ltrim", (DL_FUNC) &_HVcpp_cpp_ltrim, 2},
    {"_HVcpp_s_trim", (DL_FUNC) &_HVcpp_s_trim, 2},
    {"_HVcpp_cpp_trim", (DL_FUNC) &_HVcpp_cpp_trim, 2},
    {"_HVcpp_s_tolower", (DL_FUNC) &_HVcpp_s_tolower, 2},
    {"_HVcpp_cpp_strtolower", (DL_FUNC) &_HVcpp_cpp_strtolower, 2},
    {"_HVcpp_s_toupper", (DL_FUNC) &_HVcpp_s_toupper, 2},
    {"_HVcpp_cpp_strtoupper", (DL_FUNC) &_HVcpp_cpp_strtoupper, 2},
    {"_HVcpp_s_strlen", (DL_FUNC) &_HVcpp_s_strlen, 1},
    {"_HVcpp_cpp_strlen", (DL_FUNC) &_HVcpp_cpp_strlen, 1},
    {"_HVcpp_s_explode", (DL_FUNC) &_HVcpp_s_explode, 2},
    {"_HVcpp_cpp_explode", (DL_FUNC) &_HVcpp_cpp_explode, 2},
    {"_HVcpp_s_implode", (DL_FUNC) &_HVcpp_s_implode, 2},
    {"_HVcpp_cpp_implode", (DL_FUNC) &_HVcpp_cpp_implode, 2},
    {"_HVcpp_s_str_replace", (DL_FUNC) &_HVcpp_s_str_replace, 3},
    {"_HVcpp_cpp_str_replace", (DL_FUNC) &_HVcpp_cpp_str_replace, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_HVcpp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
