

prep.evalKey = function(key)
	{
	key = str.replace('"', "", key);
	key;	
	}
	
	

prep.evalValue = function(value)
	{
	nv = length(value);
	if(is.character(value) && nv==1) 
		{ 
		value = paste0('"',value,'"'); 
		} else { 
				value = deparse(value);
				}
	value;
	}

eval.fromTemplate = function(TEMPLATE, key, value)
	{
	TEMPLATE = str.replace("{key}", key, TEMPLATE);
	
	value = prep.evalValue(value);
	
	# str.replace failed here trying to be smart ... force=1
	TEMPLATE = gsub("{value}", value, TEMPLATE, fixed=TRUE);
	
	eval(parse(text=TEMPLATE));
	}


