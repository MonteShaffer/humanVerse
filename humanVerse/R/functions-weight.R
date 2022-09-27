



# weight.cm2ft(ft.string=FALSE)

convert.weight = function(..., from="lbs-oz", to="kgs", lb.string = TRUE, lb.digits=0, r.digits=3, lb.sep=NULL, lb.template="{x} lbs. {y} oz.")
	{
	seps = c("pounds", "lbs", "lb", "l", "'", "-", ":", ",", "^", ".");
	FROM	= prep.arg(from, n=2, keep="-", case="lower");  
	TO 		= prep.arg(to, 	 n=2, keep="-", case="lower");
	if(FROM %in% c("lb-oz", "lb-ou", "po-oz", "po-ou", "en", "uk"))
		{
		DEFAULT = c("10lbs12oz", "6lbs5oz", "5lbs8oz", "5 lbs 9 oz", "6 lbs. 5 ounces");
		} else {
				DEFAULT = c(4.876, 2.863, 2.495, 2.523, 2.865);
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
				oz_ = str.trimFromAny(oz_, "ounces oz", "right");		
				# oz_ = str.trim(oz_);
			oz_ = as.numeric(oz_);
			
			lbs_ = lb_ + oz_/16;  # 16 ounces in pound ... correct?
			## pound (avoirdupois) (lb)
			
			
			res = 0.4535924 * lbs_;
			} else {
					x = as.numeric(x);
					res = 0.4535924 * x;
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
	
	
	
	
	lbs_ = y/0.4535924;
	
	if(!lb.string) { return ( round( lbs_, r.digits) ); }
	
	lb_ = as.integer(lbs_);
	oz_ = round( 16*(lbs_ - lb_) , ft.digits);
	res = lb.template;
	# works in two calls, as expected ... 
	res = str.replace("{x}", lb_, res);
	res = str.replace("{y}", oz_, res)
	
	# res = paste0(ft_, sep, in_);
	res;
	}




	
	
weight.pounds2kilos  = function(...) { convert.weight(..., from="lb-oz", to="kg"); }
weight.pounds2grams = function(...) { convert.weight(..., from="lb-oz", to="grams"); }


weight.kilos2pounds  = function(...) { convert.weight(..., from="kg",  to="lb-oz"); }
weight.grams2pounds = function(...) { convert.weight(..., from="gram", to="lb-oz"); }



