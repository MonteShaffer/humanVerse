
warning.clear = function()
	{
	# https://stackoverflow.com/questions/5725106/r-how-to-clear-all-warnings
	assign("last.warning", NULL, envir = baseenv());
	# unlockBinding("last.warning", baseenv());
	# If warn is zero (the default), a read-only variable last.warning is created.
	}

warnings.clear = warning.clear;
clear.warnings = warning.clear;
clear.warning = warning.clear;







# https://stackoverflow.com/questions/9596918/r-warning-wrapper-raise-to-parent-function
cat.warning = function(..., sep=" ")
	{
	str = prep.dots(...);
	res = paste0(str, collapse=sep);
	res = cat.color(res);
	
	parent.call = sys.call(sys.nframe() - 1L);
	res = paste("In", deparse(parent.call), ":", res);
	# res = str.wrap(); 
	warning( res , call.=FALSE);	
	}



cat.stop = function(..., sep=" ")
	{
	str = prep.dots(...);
	res = paste0(str, collapse=sep);
	res = cat.color(res);
	
	parent.call = sys.call(sys.nframe() - 1L);
	stop( paste("In", deparse(parent.call), ":", res) , call.=FALSE);	
	}


cat.norm = function(str, open=TRUE, set.as.wd = FALSE)
	{
	nstr = normalizePath(str);
	# nstr = cat.color(nstr);
	cat( nstr );
	if(open) { utils::browseURL(nstr); }
	if(set.as.wd) { setwd( nstr ); }
	}




cat.init = function()
	{
	# load objects, where to attach?
	
	# [ESC]
	# colors8, colors16, colors256
	# <i><b><fg color="#ff0000"><bg color="black"></bg></fg></b></i><reset />
	# use HTML-like tags ... 
	# <esc custom="a" more="b"></esc>  # a generic ESC event 
	# COLOR is attached to either <fg> or <bg>
	}
 
 

log.cmd = function(what = "ping", id, res, open=FALSE)
	{
	f = "C:/_R_/-humanVerse-/SYSTEM/log/cmd/";
	p = paste0(f, what, "/");
	p = check.dir(p);
	
	if(open) { openSesame(p); }
	
	# /cmd/ping/id.log 
	o = paste0(p, id, ".log");
	o = check.file(o); # touch it ... 
	cat.log( o, what );
	cat.log( o, "");
	cat.log( o, id );
	cat.log( o, ""); 
	
	cat.dput(res, o);
	cat.log( o, ""); # end with \n 
	invisible(o);
	} 
   
cat.log = function(file, str, sep="\n", append=TRUE)
	{
	# ... logic is lost ... 
	cat(str, file=file, sep=sep, append=append);		
	}
	  
  
cat.dput = function(obj, file, one.line=FALSE, sep="\n", append=TRUE)
	{
	str = capture.output(dput(obj));
	if(one.line) { str = paste0(str, collapse=""); }
	cat(str, file=file, sep=sep, append=append);
	}
	
	# cat.dput(THIS, log);
	# dput( THIS, file=log, append=TRUE);
	# Error in dput(THIS, file = log, append = TRUE) : 
  # unused argument (append = TRUE)

 
.cat = function(..., sep="\n\n", psep = "", indent=0, color=TRUE)
	{
	strs = prep.dots(...);
	strs = paste0(strs, sep=psep); # unaltered normally?
	if(indent > 0)
		{
		pads = str.rep(" ", indent);
		n = length(strs);
		res = character(n);
		for(i in 1:n)
			{
			lines = str.explode("\n", strs[i]);
			lines = paste0(pads, lines);
			res[i] = str.implode("\n", lines);
			}
		strs = res;
		}
	if(color) { strs = cat.color(strs); }
	cat(sep);	cat(strs);	cat(sep);	
	}


# ascii vs ansi art ...
# https://github.com/syntax-samurai/ryu/blob/master/ryu.color.ansi


# if you are on CONSOLE, you shouldn't have normal HTML?
# I am going to STRIP it ... 
# maybe <i>hello</i> becomes <Hi>hello</Hi>
# how to avoid collisions ??  
# loses portability ... just strip my tags ...
# strip.Htags(str)
# 
cat.color = function(str, use.color.if.available=TRUE)
	{
	if(!use.color.if.available) { return ( strip.Htags(str) ); }
	
	# is color available ??? ... has.ansi ... 
	
	
	strip.Htags(str);
	}
	
	
# ansi256(col2rgb("pink"))
# his mapping is very strange ...
# scale as v.norm ... from 0,255 to 0,5 ... round = TRUE
## ansi256() in ansi-256.r 
## fgcodes <- c(paste0('\x1b[38;5;', 0:255, 'm'), '\x1b[39m')
## bgcodes <- c(paste0('\x1b[48;5;', 0:255, 'm'), '\x1b[49m')
## ansi256_rgb_index ... nearest rgb to each ansi color ...
## https://en.wikipedia.org/wiki/ANSI_escape_code
## # https://en.wikipedia.org/wiki/FIGlet  ascii .
## https://lists.gnu.org/archive/html/bug-ncurses/2020-03/msg00025.html
## WIKI 96 => #875f87
## ANSI-256|97|#875F87|ANSI.96|135|95|135|255
## need to zero-pad the names ... ANSI.096
## WIKI shows "default" 3-4 bit palettes by TERM ...
## ansi-256 vs ansi-16 ... ansi-16 has VENDOR palettes ... 
## reset = 0 ... bold = 1 ... italic = 3 ... underline = 4 
## notbold = 2 ... blink = 5 (slow) ... blink = 6 (fast)
## strikethrough = 9 ... 
## 30-37 set fg (ansi-16) ... 40-47 set bg (ansi-16)
## 38 set fg (ansi 256) .. 48 set bg (ansi 256)
## # set underline color/overline, and so on ... 
## super/sub 73 & 74 in mintty ... 
## https://github.com/mintty/mintty/wiki/Tips#text-attributes-and-rendering
## debian mintty terminal ???

	
## testit(char="humanVerse", width=8, style=3)
## ?textProgressBar ... 
## didin't I write my own a long time ago?
## just using \r ... width behaves funny ...
## 	
	
ansi.type = function()
	{
	0;  # no color   ANSI_0 = 0; ANSI_16 = 16; ANSI_256 = 256; ANSI_HEX = 16777216;  
	16; # ansi-16   ... special escpae codss
	256; # ansi-256 ... escape codes 
	16777216; # rgb format ... any rgb ... 
	}
	
strip.Htags = function(...)
	{
	# A-003 ... ANSI(256) colors 
	# red ... lower case base::colors()
	# wsu:crimson ... custom list 
	# A-03 ... ANSI (16) colors ... you can set a "palette" ...
	# ? why ... palette ... 
	# CFF (3 will work)
	# #abcdef will work 
	# ABCDEF will work ... 
	# convert to HEX ... figure out ANSI setup, convert to code 
	# find nearest color ... anchored to the code ... 
	# use VGA as default ANSI template ...
	# what was green/amber colors on a i286?
	## VGA does +85 on RGB to a color ... what is that 
	# dsc_0089.jpeg ... dsc_0045.jpeg 
	# blue with amber ? 000099
	# green = list("Black" = "#181e22", "Green" = "#237a53", "BrightGreen" = "#39ff8d");
	# amber = list("Black" = "#574d4c", "Amber" = "#b9662f", "BrightAmber" = "#ff9229");
	# green: black (000033) regular (336666) highlight (99ff99)
	# amber: black (333300) regular (cc9933) highlight (ffcc33) 
	## amber = dark   ( cc6633 )
	## amber = bright ( ff9933 )
	## WP 6.0 colors ... 
	## bg blue (000099) red (990000) gray (999999) black (000000) white (ffffff) cursor orange (996600) ... only six colors?
	# psd is ridiculous ...
	# https://colorpic.en.softonic.com/download
	# LOTUS (1-2-3) ... lime (00ff00) ... 009B00 (not so darK0, light blue (00A8A8) ... blue (0000A8) ... gray (373737) ... 
	# lotus = list("Black" = "#050505", "Green" = "#009B00", "BrightGreen" = "#00ff00", "Blue" = "#0000A8", "BrightBlue" = "#00A8A8", "Gray" = "#373737");
	# WP 51 dos ... blue (0000AF) ... (00A8AF) ... American Fark
	# WP 5 ... yello FBFB53 ... purplbe? A100B4 ... pink (?) teal (00A8AF) 
	# https://archive.org/download/msdos_shareware_fb_VMSYS30/msdos_shareware_fb_VMSYS30.gif
	# vmsys, green = 55ff55, darkgreen = 02aa03, yellow = ffff55, lightblue = 55ff55, teal = 00aaaa, red = ff5555, gray = aaaaaa, 
## https://colorswall.com/palette/171
## https://colorswall.com/palette/2056
## reference by name or number ... create generic names for missing values from "colors()" match ... 
## web names ... R names ...
## https://htmlcolorcodes.com/color-names/
## web 140 are more standard ? 
## The World Wide Web Consortium (W3C) has listed 16 valid color names for HTML and CSS: aqua, black, blue, fuchsia, gray, green, lime, maroon, navy, olive, purple, red, silver, teal, white, and yellow.
### 12, 40, other colors ... https://onlymyenglish.com/colors-name-english/
## http://www.jennyscrayoncollection.com/2020/11/complete-list-of-faber-castell.html
## color penciles ... 	

# WP, what is this, pattern?? very few aa and cc 
# 00 44 88 ff ... 3 in a pair, how may combos?
# base2int("0f","44","88","ffff", base=16)
# + 68 on integer, going to 272=>255 
# ## CC is that trajectory ... 
# what aboout 5 equal groups ... 0 n1 n2 n3 255 
# c(0, 64, 128, 192, 256);
# 00, 40, 80, C0, FF 
# c(0, 51, 102, 153, 204, 255);
# 00, 33, 66, 99, CC, FF ... websafe ... 
# A8 = 168 
# 0, 68, 136, 204, 272=>255 
# 00 44   88   cc   ff  ... 48?   4*4*4*4 = 256 ... 3x3x3 = 27 
# 4x4x3 ? 
# ff8888, ffff88, 88ff88, 00ff88, 88ffff, 0088ff, ff88cc, ff88ff
# ff0000, ffff00, 88ff00, 00ff44, 00ffff, 0088cc, 8888cc, ff00ff
# 884444, ff8844, 00ff00, 008888, 004488, 8888ff, 880044, ff0088
# 880000, ff8800, 008800, 008844, 0000ff, 0000aa, 880088, 8800ff
# 440000, 884400, 004400, 004444, 000088, 000044, 440044, 440088
# 000000, 888800, 888844, 888888, 448888, cccccc, 440044, ffffff
{
	# WP 48 ... 8 x 6 ... f8877e, f8ff81, 81ff81, 00ff81, 81ffff, 0380e6, ff7fc1, ff7dff
	# ff0000, ffff00, 81ff00, 00ff38, 00fffa, 027fc9, 8482c7, ff00ff,
	# 7e4046, fe8242, 00fd02, 008282, 024383, 8182f8, 7c033c, ff0283, 
}
	# 790a00, fd8500, 008400, 00843b, 0000ff, 04029e, 820082 (purple), 8300ff
	# 390201, 823a08, 023c00, 004448, 030279, 02013d, 3c003c, 450080
	# 010000, 808200, 81813b, 7e8180, 3b807f (teal), bfc3c7,  3f013d, ffffff
	
	str = prep.dots(..., 
		default = 'Wel<bg = "yellow">come</bg> <v>to</v> the <color fg BrightGreen bg = A003 ><fg= red         >human</fg>Verse!</color>' );
		
	co = c(0, 3, 1, 4, 1, 5, 9); # codes of SIMPLE 
	st = c("v", "i", "b", "u", "b", "blink", "s"); 	# SIMPLE
	ns = c("fg", "bg", "color");				# *NOT* SIMPLE
	# return(gsub("<.*?>", "", str));
	
	n = length(st);
	tags = character();
	for(i in 1:n)
		{
		otag = paste0("<", st[i],">");
		ctag = paste0("</",st[i],">");
		tags = c(tags, otag, ctag);
		}
	str = str.replace(tags, "", str);
	
	m = length(ns);
	for(j in 1:m) 
		{
		# https://stackoverflow.com/a/18464575/184614
		p = paste0("<\\/?",ns[j],"[^>]*>");
		str = gsub(p, "", str);
		}
	str;	
	}

# https://stackoverflow.com/a/11365002/184614
# Jokes apart from this, don't try to parse HTML with Regex, use a HTML parser. It will make your life easy.
# https://stackoverflow.com/a/1732454/184614
# you can't vote on locked posts ... 	






 

check.colorCapability = function()
	{
	
# https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r

	# term <- Sys.getenv()["TERM"]
  # colour_terms <- c("xterm-color","xterm-256color", "screen", "screen-256color")

  # if(rcmd_running() || !any(term %in% colour_terms, na.rm = TRUE)) {
    # return(text)
  # }
	
	}
