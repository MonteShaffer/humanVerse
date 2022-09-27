



# weight.cm2ft(ft.string=FALSE)

convert.weight = function(..., from="lbs-oz", to="kgs", lb.string = TRUE, lb.digits=0, r.digits=3, lb.sep=NULL)
	{
	seps = c("pounds", "lbs", "lb", "l", "'", "-", ":", ",", "^", ".");
	FROM	= prep.arg(from, n=2, keep="-", case="lower");  
	TO 		= prep.arg(to, 	 n=2, keep="-", case="lower");
	if(FROM %in% c("lb-oz", "lb-ou", "po-oz", "po-ou", "en", "uk"))
		{
		DEFAULT = c("10lbs12oz", "6lbs5oz", "5lbs8oz", "5 lbs 9 oz", "6 lbs. 5 ounces");
		} else {
				DEFAULT = c(1.22, 1.65, 1.73, 1.75, 1.95);
				# grams
				if(FROM %in% c("gr", "g")) { DEFAULT = DEFAULT*1000;  } 
				# centigrams
				if(FROM %in% c("cg", "ce")) { DEFAULT = DEFAULT*1000*100;  } 
				# milligrams
				if(FROM %in% c("mg", "mi")) { DEFAULT = DEFAULT*1000*1000; } 	
				}
				
	
	x = prep.dots(..., default=DEFAULT);
	
	
	if(FROM %in% c("lb-oz", "lb-ou", "po-oz", "po-ou", "en", "uk"))
		{
		if(is.character(x) || lb.string)
			{
			x = as.character(x);
			if(is.null(lb.sep)) { sep = smart.sep(x, seps); } else { sep = lb.sep; }
			y = check.list(str.explode(sep, x));
			
			# need to cleanup before as.numeric 
			lb_ = list.getElements(y, 1);
				# lb_ = str.trim(lb_);
				# this multivariate noise cleanse still doesn't work 
				# need to merge the singletons on the search ...
				# if anyNOT NA ... left or RIGHT ...
				lb_ = str.trimFromAny(lb_, "pounds lbs", "both");
			lb_ = as.numeric(lb_);
			oz_ = list.getElements(y, 2); 
				# singletons seem to work ...
				oz_ = str.trimFromAny(oz_, ". ", "left");
				# shouldn't need to trim if we include " " in Any
				oz_ = str.trimFromAny(oz_, "ounces oz", "left");		
				# oz_ = str.trim(oz_);

			
			res = 12*0.0254*ft_ + 0.0254*in_;
			} else {
					x = as.numeric(x);
					res = 12*0.0254*x;
					}
		
		res = round(res, r.digits);
		
		# grams
		if(TO %in% c("gr", "g")) { return( 1000 * res );  } 
		# centigrams
		if(TO %in% c("cg", "ce")) { return( 1000*100 * res );  } 
		# milligrams
		if(TO %in% c("mg", "mi")) { return( 1000*1000 * res ); } 	
		return(res);  # DEFAULT is kilograms  
		}
	
	## we have cg, mg or g ... convert to kilos 
	y = x;
	# grams
	if(FROM %in% c("gr", "g")) { y = x/1000;  } 
	# centigrams
	if(FROM %in% c("cg", "ce")) { y = x/(1000*100);  } 
	# milligrams
	if(FROM %in% c("mg", "mi")) { y = x/(1000*1000); } 	
	
	
	
	if(is.null(ft.sep)) { sep = seps[1]; } else { sep = ft.sep; }
	
	ft = y / (12*0.0254);
	if(!ft.string) { return ( round( ft, r.digits) ); }
	ft_ = as.integer(ft);
	in_ = round( ft - ft_ , ft.digits);
	res = paste0(ft_, sep, in_);
	res;
	}




	
	
weight.ft2m  = function(...) { convert.weight(..., from="ft-in", to="m"); }
weight.ft2cm = function(...) { convert.weight(..., from="ft-in", to="cm"); }
weight.ft2mm = function(...) { convert.weight(..., from="ft-in", to="mm"); }


weight.m2ft  = function(...) { convert.weight(..., from="m",  to="ft-in"); }
weight.cm2ft = function(...) { convert.weight(..., from="cm", to="ft-in"); }
weight.mm2ft = function(...) { convert.weight(..., from="mm", to="ft-in"); }



