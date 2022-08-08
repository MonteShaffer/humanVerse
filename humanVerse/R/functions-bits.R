

##################################################
#'
#' bit.RShift
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for internal R 'md5_' computation, and has some issues.
#' Maybe R::CRAN will fix this someday in the base?
#'
#' @param x VECTOR of integers to be shifted
#' @param bits How far to shift [non-negative integer vector of values up to 31.]
#' @param method Use C++ is library(HVcpp) or Rcpp::sourceCpp(str.cpp)
#' @param unsigned USED for INTERNAL R hack
#'
#' @return
#' @export
#'
#' @examples
#' bit.RShift( c(1732584193,-1732584193,1732,-1732), 16, method="cpp"); # 26437, -26438, 0, -1
#' bit.RShift( c(1732584193,-1732584193,1732,-1732), 16, method="base");		
#' 
bit.RShift = function(x, bits=1, method="cpp", unsigned=FALSE)
	{
	if(isFALSE(bits %in% 1:31))
		{
		msg = paste0("\n\n", str.commentWrapper(paste0(" bits = ", bits," is OUT OF RANGE ")), "\n\n",
					"You should try an INTEGER in range 1:31 ", "\n\n",
					"Results may be SPURIOUS", "\n");
		warning(msg);
		}

	# necessary overhead
	m = functions.cleanKey(method, 1);

	if(m == "c" && exists("bits_RShift"))
		{
		return( bits_RShift(x, bits) );
		} 

		
	res = bitwShiftR(x,bits);
		x.w = (!is.negative(x) | unsigned);
		x.idx = which(x.w == FALSE);
	res[ x.idx ] = -bitwShiftR(-x[x.idx],bits) - 1;
	res;
	}

#' @rdname bitShiftR
#' @export
bitShiftR = bit.RShift;







##################################################
#'
#' bit.LShift
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for internal R 'md5_' computation, and has some issues.
#' Maybe R::CRAN will fix this someday in the base?
#'
#' @param x VECTOR of integers to be shifted
#' @param bits How far to shift [non-negative integer vector of values up to 31.]
#' @param method Use C++ is library(HVcpp) or Rcpp::sourceCpp(str.cpp)
#' @param unsigned USED for INTERNAL R hack
#'
#' @return
#' @export
#'
#' @examples
#' bit.LShift( c(1732584193,-1732584193,1732,-1732), 16, method="cpp"); # [1]  1.14e+14 -1.14e+14  1.14e+08 -1.14e+08
#' bit.LShift( c(1732584193,-1732584193,1732,-1732), 16, method="base");		
#'
bit.LShift = function(x, bits=1, method="cpp", unsigned=FALSE)
	{
	if(isFALSE(bits %in% 1:31))
		{
		msg = paste0("\n\n", str.commentWrapper(paste0(" bits = ", bits," is OUT OF RANGE ")), "\n\n",
					"You should try an INTEGER in range 1:31 ", "\n\n",
					"Results may be SPURIOUS", "\n");
		warning(msg);
		}

	# necessary overhead
	m = functions.cleanKey(method, 1);

	if(m == "c" && exists("bits_LShift"))
		{
		return( bits_LShift(x, bits) );
		} 

		
	res = bitwShiftL(x,bits);
		x.w = (!is.negative(x) | unsigned);
		x.idx = which(x.w == FALSE);
	res[ x.idx ] = -bitwShiftL(-x[x.idx],bits);
	res;
	}

#' @rdname bitShiftL
#' @export
bitShiftL = bit.LShift;








# https://stackoverflow.com/questions/64839024/
# https://stackoverflow.com/questions/37121897/
# Rcpp::cppFunction("long long RShift(long long a, int b) { return a >> b;}");
# Rcpp::cppFunction("long long LShift(long long a, int b) { return a << b;}");
####### Rcpp::cppFunction("long long ShiftR(long long a, int b) { return a >> b;}");
####### Rcpp::cppFunction("long long ShiftL(long long a, int b) { return a << b;}");

# https://cran.r-project.org/web/packages/Rcpp/vignettes/Rcpp-package.pdf
####### Rcpp::cppFunction("long long ShiftR(long long a, int b) { return a >> b;}");
####### Rcpp::cppFunction("long long ShiftL(long long a, int b) { return a << b;}");
#' ShiftR(1732584193, 16);		# 26437
#' ShiftR(-1732584193, 16);		# -26438
# http://adv-r.had.co.nz/Rcpp.html
# https://www.geeksforgeeks.org/bitwise-operators-in-c-cpp/
# https://stackoverflow.com/questions/34011318/in-c-can-you-use-a-dot-in-variable-or-function-names/34011346
####### Rcpp::cppFunction("long long bit_shiftR(long long a, int b) { return a >> b;}");
####### Rcpp::cppFunction("long long bit_shiftL(long long a, int b) { return a << b;}");
####### Rcpp::cppFunction("long long bit_OR(long long a, long long b) { return a | b;}");
####### Rcpp::cppFunction("long long bit_XOR(long long a, long long b) { return a ^ b;}");
####### Rcpp::cppFunction("long long bit_AND(long long a, long long b) { return a & b;}");
####### Rcpp::cppFunction("long long bit_NOT(long long a) { return ~ a ;}");

# convert from bits to integer and reverse 
# https://www.geeksforgeeks.org/convert-an-integer-to-bits-in-r-programming-inttobits-function/#:~:text=Convert%20an%20Integer%20to%20Bits%20in%20R%20Programming%20%E2%80%93%20intToBits()%20Function&text=intToBits()%20function%20in%20R,the%20length%20of%20integer%20vector.
# base::intToBits ... type = raw ... 
# https://stackoverflow.com/questions/6614283/converting-decimal-to-binary-in-r

# these are the PHP names ... 
decbin <- function(fnum) {
  bin_vect <- rep(0, 1 + floor(log(fnum, 2)))
  while (fnum >= 2) {
    pow <- floor(log(fnum, 2))
    bin_vect[1 + pow] <- 1
    fnum <- fnum - 2^pow
  } # while
  bin_vect[1] <- fnum %% 2
  paste(rev(bin_vect), collapse = "")
} #dec2bin


  # http://php.net/manual/en/function.hexdec.php
  # http://php.net/manual/en/function.dechex.php
  
  # https://www.mathworks.com/help/matlab/ref/bin2dec.html
  # bin2dec is matlab
  # bindec is PHP 
  
  # Convert text representation of binary integer to double value
  

bindec = function(binstr)
	{
	n = strlen(binstr);
	res = 0; power = 0;
	for(i in n:1)
		{
		bit = as.integer(charAt(binstr,i));
		add = 0;
		if(bit == 1) { add = 2^power; }
		
		res = res + add;
		power = 1 + power;
		}
	res;
	}



dec2bin = function(decnum) 
	{
	bvect = rep(0, 1 + floor(log(decnum, 2))); # pre-populate with zeroes
	while (decnum >= 2) 
		{
		power = floor(log(decnum, 2));
		bin_vect[1 + power] = 1;
		decnum = decnum - 2^power;
		} 
	bvect[1] = decnum %% 2;
	paste(rev(bvect), collapse = ""); # convert to a string
	} 


#' bitShiftR
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for manual 'md5_' computation, and has some issues.
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
#' bitShiftR(1732584193, 16);		# 26437
#' bitShiftR(-1732584193, 16);		# -26438
# bit.shift.right
bitShiftR = function(x, bits, unsigned=FALSE)
  {
  if(!is.negative(x) | unsigned) { return( bitwShiftR(x,bits) ); }
  -bitwShiftR(-x,bits) - 1; #  - 1;                  # >>>
  # https://stackoverflow.com/questions/64839024/using-r-how-to-do-bitwise-shifting-for-signed-negative-integers
  # maybe ... Rshift <- function(val, nbits) floor(val/2^nbits)
  }

#' bitShiftL
#'
#' This updates the built-in functions to allow for negative integers.
#' Used for manual 'md5_' computation, and has some issues.
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
# bit.shift.left
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
#' Used for manual 'md5_' computation, and has some issues.
#' Maybe R::CRAN will fix this someday in the base?
#'
#' Specifically, this addresses overflows ...
#'
#' @param a integer 'a'
#' @param b integer 'b'
#'
#' @return
#' @export
#'
#' @examples
#' bitOr(15, 7);
# bit.or
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





