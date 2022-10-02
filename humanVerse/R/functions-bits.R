  
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
.NOT. = function(a, b=NULL) 
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
