
# https://stackoverflow.com/questions/64839024/
# https://stackoverflow.com/questions/37121897/
# Rcpp::cppFunction("long long RShift(long long a, int b) { return a >> b;}");
# Rcpp::cppFunction("long long LShift(long long a, int b) { return a << b;}");
####### Rcpp::cppFunction("long long ShiftR(long long a, int b) { return a >> b;}");
####### Rcpp::cppFunction("long long ShiftL(long long a, int b) { return a << b;}");





#' bitShiftR
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for manual '.md5' computation, and has some issues.
#' Maybe R::CRAN will fix this someday in the base?
#'
#' @param x integer
#' @param bits bits
#' @param unsigned is it signed or not
#'
#' @return new integer
#' @export
#'
#' @examples
#' bitShiftR(1732584193, 16);
#' bitShiftR(-1732584193, 16);
bitShiftR = function(x, bits, unsigned=FALSE)
  {
  if(!is.negative(x) | unsigned) { return( bitwShiftR(x,bits) ); }
  -bitwShiftR(-x,bits) - 1; #  - 1;                  # >>>
  }

#' bitShiftL
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for manual '.md5' computation, and has some issues.
#' Maybe R::CRAN will fix this someday in the base?
#'
#' @param x integer
#' @param bits bits
#' @param unsigned is it signed or not
#'
#' @return new integer
#' @export
#'
#' @examples
#' bitShiftL(1732, 16);
#' bitShiftL(-1732, 16);
bitShiftL = function(x, bits, unsigned=FALSE)
  {
  if(!is.negative(x) | unsigned)
    {
    tmp = suppressWarnings( bitwShiftL(x,bits) );                # <<<
    if(is.na(tmp)) { tmp = -2^31; }  # 0x80 << 24
    return( tmp );
    }
  tmp = suppressWarnings( -bitwShiftL(-x,bits) ); # - 1;                  # <<<
  if(is.na(tmp))
    {
    tmp = 2^31;
    if(is.negative(x)) { tmp = -1 * tmp; }
    }
  tmp;
  }


#' bitOr
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for manual '.md5' computation, and has some issues.
#' Maybe R::CRAN will fix this someday in the base?
#'
#' Specifically, this addresses overflows ...
#'
#' @param a integer 'a'
#' @param b integer 'b'
#'
#' @return
#' @export
#' bitOr(15, 7);
bitOr = function(a, b)
  {
  if(!is.negative(a) && ( b <= -1 * 2^31) )
    {
    return (a + b);
    }
  if(!is.negative(b) && ( a <= -1 * 2^31) )
    {
    return (a + b);
    }
  bitwOr(a,b);
  }





