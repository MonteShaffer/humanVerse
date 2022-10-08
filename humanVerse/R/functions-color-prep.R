

color.css = function(fname = "C:/_R_/cnames.html")
	{
	res = memory.get("css-colors", "-COLORS-");
	if(INN(res)) { return(res); }
	furl = "https://htmlcolorcodes.com/color-names/index.html";
	if(!file.exists_(fname))
		{ curl::curl_download(furl, fname); }
		# download.file on THIS fails miserably ... 
	str = readTextFile(fname);
	
	L = '<section id="';
	R = '</section>';

	sections = str.slice(L, str, R, keep=FALSE);
	classes  = v.truncate( 
					str.before(DOUBLE_QUOTE, sections, 1), 10);
	
	ns = length(classes);
	
	df = NULL;
	for(i in 1:ns)
		{
		class = classes[i];
					L = '<tr class="color-table__row">';
					R = '</tr>';
		colors = v.empty(str.slice(L, sections[i], R), 1);
		nc = length(colors);
		for(j in 1:nc)
			{
			color = colors[j];
			
					L = 'data-hex="';
					R = DOUBLE_QUOTE;
			hex = toupper(str.slice(L, color, R, keep=FALSE));
			
					L = '<td class="color-table__cell color-table__cell--name">';
					R = '</td>';

			cname = str.slice(L, color, R, keep=FALSE);

			df = rbind(df, df.row( class, hex, cname ) );
			}
		}
	
	colnames(df) = c("color.class", "color.hex", "color.name");
	df = property.set("src", df, "https://htmlcolorcodes.com/color-names/");
	
	memory.set("css-colors", "-COLORS-", df);
	
	df;	
	}


# this is redundant of 'color.default'
color.buildPalette = function(HEX, cnames = "", 
								library="css", type="RGB",
									scale.RGB = TRUE)
	{
	HEX = color.hex( HEX );
	n   = length(HEX);
	np  = str.pad(1:n, strlen(max(n)), "0", "LEFT");
	if(cnames == "") { cnames = paste0(library,".",np); }
	
	# type = "CMYK-HSL" # we build RGB no matter what ...
	
	TYPE 	= prep.arg(type, n=3, keep="-", case="upper");
	mkey 	= .MD5( paste0(library, 
						paste0(HEX, cnames, collapse=""),
						TYPE, as.character(scale.RGB )) );
						
	df		= memory.get(mkey, "-COLORS-");
	if(INN(df)) { return(df); } 
	
	
	RGB = color.convert(HEX, from="HEX", to="RGB");
	XXX = NULL;
	TYPES = v.remove( str.explode("-", TYPE), "RGB" );
	if(INN(TYPES))
		{
		XXX = list();
		# may have multiple, I want cmyk, hsl 
		nt = length(TYPES);
		for(i in 1:nt)
			{
			XXX[[i]] = color.convert(RGB, from=RGB, to=TYPES[i]);
			}
		}
	
	if(scale.RGB) { RGB = RGB / 255; } 
	
	rtype = typeof(RGB);
	
	df = dataframe( cbind( colors, HEX ) );
	df = cbind(df,	as.type(RGB[1,], type=rtype), 
					as.type(RGB[2,], type=rtype), 
					as.type(RGB[3,], type=rtype) );

	xnames = NULL;
	if(INN(XXX))
		{
		xnames = NULL;
		for(i in 1:nt)
			{
			rxnames = rownames(XXX[[i]]);
			xnames = c(xnames, rxnames);
			nj = length(rxnames);
			njt = typeof(XXX[[i]][1,]);
			for(j in 1:nj)
				{
				df = cbind(df, as.type(XXX[[i]][j,], type=njt));
				}
			}
		}
				
	rownames(df) = 1:n;
	colnames(df) = c("color", "hex", 
						"red", "green", "blue", xnames);
	
	df = property.set("library", df, library);
	df = property.set("md5", df, mkey);
	memory.set(mkey, "-COLORS-", df);
	df;
	}
	
	

color.buildMap = function(df=color.css(), order=c(1,2,3), 
									library="css", parent="")
	{
	# if order element is zero, missing ... 
	# ORDER = color.class, color.hex, color.name 
	# e.g., order = c(0,1,0) means I only have HEX values ...
	# df = color.css();  # 3 columns ... 
	# .MD5(fn.args) ... would be nice ... 
	# "699cdc05c57e9ffa4f67220dbe1d8a75" 
	mkey 	= .MD5( paste0(library, parent, paste0(order, collapse=""), paste0(df,collapse="") ) );
						
	df		= memory.get(mkey, "-COLORS-");
	if(!is.null(df)) { return(df); } 
	
	
	n = nrow(df);
	np = str.pad(1:n, strlen(max(n)), "0", "LEFT");
	
	if(order[1] != 0) { class = df[, order[1] ]; }
	if(order[1] == 0) { class = str.rep("", n); }
	
	if(order[3] != 0) { cnames = df[, order[3] ]; }
	if(order[3] == 0) { cnames = paste0(library,".",np); }
	
	HEX = color.hex( df[, order[2] ] );
	RGB = color.convert(HEX, from="HEX", to="RGB");
	
	# nearest from R palette (distinct)
	RNN = character(n);
	for(i in 1:n)
		{
		hex = HEX[i];
		R = as.character( color.nearest(hex, n=1) );
.cat("\t i: ", i, " \t\t hex: ", hex, " \t\t R: ", R);
flush.console(); 
		RNN[i] = R;
		}
	
	# nearest from CSS palette (currently THIS)
	CNN = character(n);
	
	
	
	
	}

