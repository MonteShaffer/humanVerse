


eval.fromTemplate = function(TEMPLATE, key, value)
	{
	TEMPLATE = str.replace("{key}", key, TEMPLATE);
	
	value = prep.evalValue(value);
	
	# str.replace failed here trying to be smart ... force=1
	TEMPLATE = gsub("{value}", value, TEMPLATE, fixed=TRUE);
	
	eval(parse(text=TEMPLATE));
	}


