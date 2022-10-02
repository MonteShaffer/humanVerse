
# a >> b; ... a %>>% b;
# unsigned ?
## multivariate on a 
.SHIFT_R. = function(a, bits=16) 
	{
	# INTEGER_MAXIMUM		= 2147483647
	# a = c(1732584193, -1732584193, 2147483647, -2147483647, 4611686018427387904, -4611686018427387904);
	# setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
	# Rcpp::sourceCpp('bits.cpp');
	# cpp_SHIFT_R(a,16);		# 26437 -26438 32767 -32768 70368744177664 -70368744177664
	# javascript ... 			# 26437 -26438 32767 -32768 0 0 
	# .SHIFT_R.(a, 16);			# 26437 -26438 32767 -32768 0 0 
	# bitwShiftR(a, 16);		# 26437  39098 32767  32768 NA NA 

	a_ = a;
	a = suppressWarnings( as.integer(a) );
	b = check.base(bits);
	if(is.null(b)) { stop("base issues"); }
	
	OVERFLOW.logic 		= abs(a_) > INTEGER_MAXIMUM;
	
	NEG.logic 			= is.negative(a_)
	res 				= bitwShiftR(a, b);
	res[NEG.logic] 		= -bitwShiftR(-a[NEG.logic], b) - 1;
	res[OVERFLOW.logic] = 0;
	res;
	} 


	
# a << b;   a %<<% b;
# unsigned ? 
# multivariate with error checks on R-base ...
.SHIFT_L. = function(a, bits=8) 
	{
	# INTEGER_MAXIMUM		= 2147483647
	# a = c(1732584193, -1732584193, 2147483647, -2147483647, 4611686018427387904, -4611686018427387904);
	# setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
	# Rcpp::sourceCpp('bits.cpp');
	# cpp_SHIFT_L(a,16);		# 113546637672448 -113546637672448  140737488289792 -140737488289792 0 0 
	# javascript ... 	# 587268096 -587268096 -65536 65536 0 0
	# .SHIFT_L.(a, 16);	# 587268096 -587268096 -65536 65536 0 0
	# bitwShiftL(a, 16);# 587268096 -587268096 -65536 65536 NA NA 
	#   

	a_ = a;
	a = suppressWarnings( as.integer(a) );
	b = check.base(bits);
	if(is.null(b)) { stop("base issues"); }
	
	NA.logic 			= v.test(a, NA);
	OVERFLOW.logic 		= abs(a_) > INTEGER_MAXIMUM;
	
	NEG.logic 			= is.negative(a_) & !NA.logic;
	res 				= bitwShiftL(a, b);	
	res[NEG.logic] 		= -bitwShiftL(-a[NEG.logic], b);
	res[OVERFLOW.logic] = 0;
	res;
	} 





# (a & b);
.AND. = function() {}
.AND. = function(a, b, method=DEFAULT_BIT_METHOD) 
	{
	# INTEGER_MAXIMUM		= 2147483647
	# a = c(1732584193, -1732584193, 2147483647, -2147483647, 4611686018427387904, -4611686018427387904);
	# setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
	# Rcpp::sourceCpp('bits.cpp');
	# cpp_AND(a,16);		#  0 -9223267582970118144 2147483647 -8646805731435085824 0 -9223372036854775808
	# javascript ... 	# 0 16 16 0 0 0 
	# .AND.(a, 16);		# 0 16 16 0 0 0 
	# bitwAnd(a, 16);	# 0 16 16 0 NA NA 
	#   
	
	a_ = a; b_ = b;
	a = suppressWarnings( as.integer(a) );
	b = suppressWarnings( as.integer(b) );
	
		
	
	OVERFLOW.logic 		= abs(a_) > INTEGER_MAXIMUM;
	
	res = suppressWarnings( bitwAnd(a,b));
	res[OVERFLOW.logic] = 0;
	res;
	} 




# (a | b);
.OR. = function() {}
.OR. = function(a, b, method=DEFAULT_BIT_METHOD) 
	{
	# INTEGER_MAXIMUM		= 2147483647
	# a = c(1732584193, -1732584193, 2147483647, -2147483647, 4611686018427387904, -4611686018427387904);
	# setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
	# Rcpp::sourceCpp('bits.cpp');
	# cpp_OR(a,16);		#   1732584209 -1157636353 491773755391 -1303068045 4611688686411353088 -4611545280094420480

	# javascript ... 	# 1732584209 -1732584193 2147483647 -2147483631 16 16 
	# .OR.(a, 16);		#  1732584209 -1732584193 2147483647 -2147483631 16 16
 
	# bitwOr(a, 16);	# 1732584209 -1732584193 2147483647 -2147483631 NA NA
	
	a_ = a; b_ = b;
	a = suppressWarnings( as.integer(a) );
	b = suppressWarnings( as.integer(b) );	
	alen = length(a); blen = length(b);
	if(blen < alen) 
		{ 
		# have to be same length for replacement logic at end 
		b 	= rep(b,  length.out=alen); 
		b_ 	= rep(b_, length.out=alen);
		}
	
	res = suppressWarnings( bitwOr(a,b));
		
	# for larger/smaller integers, we have a problem ...
	OVERFLOW.logicA 	= (abs(a_) > INTEGER_MAXIMUM);
	OVERFLOW.logicB 	= (abs(b_) > INTEGER_MAXIMUM);
	OVERFLOW 			= OVERFLOW.logicA & OVERFLOW.logicB;
	
	
	res[OVERFLOW.logicA] 	= b_[OVERFLOW.logicA];   # (a + b);
	res[OVERFLOW.logicB] 	= a_[OVERFLOW.logicB];   # (a + b);
	res[OVERFLOW.logicB]	= 0;
	
	res;
	} 


# (a ^ b);
.XOR. = function() {}
.XOR. = function(a, b) 
	{
	# INTEGER_MAXIMUM		= 2147483647
	# a = c(1732584193, -1732584193, 2147483647, -2147483647, 4611686018427387904, -4611686018427387904);
	# setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
	# Rcpp::sourceCpp('bits.cpp');
	# cpp_XOR(a,16);		#   1732584209 -1157636353 491773755391 -1303068045 4611688686411353088 -4611545280094420480

	# javascript ... 	# 1732584209 -1732584193 2147483631 -2147483631 16 16 
	# .XOR.(a, 16);		#  1732584209 -1732584193 2147483631 -2147483631 16 16
 
	# bitwXor(a, 16);	# 1732584209 -1732584209  2147483631 -2147483631 NA NA
	
	a_ = a; b_ = b;
	a = suppressWarnings( as.integer(a) );
	b = suppressWarnings( as.integer(b) );	
	alen = length(a); blen = length(b);
	if(blen < alen) 
		{ 
		# have to be same length for replacement logic at end 
		b 	= rep(b,  length.out=alen); 
		b_ 	= rep(b_, length.out=alen);
		}
	
	res = suppressWarnings( bitwXor(a,b));
		
	# for larger/smaller integers, we have a problem ...
	OVERFLOW.logicA 	= (abs(a_) > INTEGER_MAXIMUM);
	OVERFLOW.logicB 	= (abs(b_) > INTEGER_MAXIMUM);
	OVERFLOW 			= OVERFLOW.logicA & OVERFLOW.logicB;
		
	res[OVERFLOW.logicA] 	= b_[OVERFLOW.logicA];   # (a + b);
	res[OVERFLOW.logicB] 	= a_[OVERFLOW.logicB];   # (a + b);
	res[OVERFLOW.logicB]	= 0;
	
	res;
	}


# (~a);
.NOT. = function() {}
.NOT. = function(a) 
	{
	# INTEGER_MAXIMUM		= 2147483647
	# a = c(1732584193, -1732584193, 2147483647, -2147483647, 4611686018427387904, -4611686018427387904);
	# setwd("C:/_git_/github/MonteShaffer/humanVerse/HVcpp/src/");
	# Rcpp::sourceCpp('bits.cpp');
	# cpp_NOT(a);		#   -1732584194 1732584192 -2147483646 2147483646 -4611686018427387904  4611686018427387904

	# javascript ... 	# -1732584194 1732584192 -2147483648 2147483646 -1 -1 
	# .NOT.(a);			# -1732584194 1732584192 -2147483648  2147483646 -1 -1 
	# bitwNot(a);		# -1732584194 1732584192 NA 2147483646 NA NA
	
	a_ = a;
	a = suppressWarnings( as.integer(a) );
	
	res = suppressWarnings( bitwNot(a) );
	NA.logic = is.na(res);
	
	OVERFLOW.logic 	= (abs(a_) > INTEGER_MAXIMUM);
	OVERFLOW.border = (a_ == INTEGER_MAXIMUM);
	# NEG.logic 		= is.negative(a_);

	res[NA.logic & OVERFLOW.border] = -1*(a_[NA.logic & OVERFLOW.border] + 1);
	res[NA.logic & OVERFLOW.logic] 	= -1;
	res;	
	} 






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	m = prep.arg(method, 1);

	if(m == "c" && exists("bits_RShift"))
		{
		return( bits_RShift(x, bits) );
		} 

		
	res = bitwShiftR(x,bits);
		x.w = (!is.na(x) & (!is.negative(x) | unsigned));
		x.idx = which(x.w == FALSE);
	res[ x.idx ] = -bitwShiftR(-x[x.idx],bits) - 1;
	res;
	}

#' @rdname bitShiftR
#' @export
bitShiftR = bit.RShift;







#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
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
	m = prep.arg(method, 1);

	if(m == "c" && exists("bits_LShift"))
		{
		return( bits_LShift(x, bits) );
		} 

		
	res = bitwShiftL(x,bits);  # this is multivariate 
		x.w = (!is.na(x) & (!is.negative(x) | unsigned));
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
  # this is NOT multivariate at the moment ... 
  if(!is.na(x) & (!is.negative(x) | unsigned)) { return( bitwShiftR(x,bits) ); }
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
  if(!is.na(x) & (!is.negative(x) | unsigned))
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





