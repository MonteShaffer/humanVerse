



# height.cm2ft(ft.string=FALSE)

convert.height = function(..., from="ft-in", to="cm", ft.string = TRUE, ft.digits=0, r.digits=3, ft.sep=NULL)
	{
	seps = c("'", "-", ":", ",", "^", ".", "ft", "f");
	FROM	= prep.arg(from, n=2, case="lower");  
	TO 		= prep.arg(to, 	 n=2, case="lower");
	if(FROM %in% c("ft", "fo", "fe", "en", "uk"))
		{
		DEFAULT = c("4'0", "5'5", "5'8", "5'9", "6'5");
		} else {
				DEFAULT = c(1.22, 1.65, 1.73, 1.75, 1.95);
				# centimeters
				if(FROM %in% c("cm", "ce")) { DEFAULT = DEFAULT*100;  } 
				# millimeters
				if(FROM %in% c("mm", "mi")) { DEFAULT = DEFAULT*1000; } 	
				}
				
	
	x = prep.dots(..., default=DEFAULT);
	
	
	if(FROM %in% c("ft", "fo", "fe", "en", "uk"))
		{
		if(is.character(x) || ft.string)
			{
			x = as.character(x);
			if(is.null(ft.sep)) { sep = smart.sep(x, seps); } else { sep = ft.sep; }
			y = check.list(str.explode(sep, x));
			ft_ = as.numeric(list.getElements(y, 1));
			# 'in' is reserved word 
			in_ = as.numeric(list.getElements(y, 2));  
			res = 12*0.0254*ft_ + 0.0254*in_;
			} else {
					x = as.numeric(x);
					res = 12*0.0254*x;
					}
		
		res = round(res, r.digits);
		
		if(TO %in% c("cm", "ce")) { return( 100 * res ); }  # centimeters
		if(TO %in% c("mm", "mi")) { return( 1000 * res ); } # millimeters
		return(res);  # DEFAULT is meters 
		}
	
	## we have cm, mm or meters ... convert to meters 
	y = x;
	if(FROM %in% c("cm", "ce")) { y = x/100;  }  # centimeters
	if(FROM %in% c("mm", "mi")) { y = x/1000; } # millimeters
	
	
	if(is.null(ft.sep)) { sep = seps[1]; } else { sep = ft.sep; }
	
	ft = y / (12*0.0254);
	if(!ft.string) { return ( round( ft, r.digits) ); }
	ft_ = as.integer(ft);
	in_ = round( 12*(ft - ft_) , ft.digits);
	
	o = (in_ == 12);
	ft_[o] = 1 + ft_[o]; # rounding up ...
	in_[o] = in_[o] - 12; 
	
	# 1.219 ... 3'12"
	
	res = paste0(ft_, sep, in_);
	res;
	}




	
	
height.ft2m  = function(...) { convert.height(..., from="ft-in", to="m"); }
height.ft2cm = function(...) { convert.height(..., from="ft-in", to="cm"); }
height.ft2mm = function(...) { convert.height(..., from="ft-in", to="mm"); }


height.m2ft  = function(...) { convert.height(..., from="m",  to="ft-in"); }
height.cm2ft = function(...) { convert.height(..., from="cm", to="ft-in"); }
height.mm2ft = function(...) { convert.height(..., from="mm", to="ft-in"); }



