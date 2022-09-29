

## build this alias list in one spot, 
## load into namespace at init() (onLoad)

alias.init = function()
	{
	# if a%*%b is slower than crossprod(a,b), why not replace it?
	# "%*%" = crossproduct;

	# BASE = list(
			
	
	
	
	INTERNAL = list(); # smart aliasing on functions ... fn. expansion 
	 }
	 
	 
	arg.prep = prep.arg; 
	prep.args = prep.arg; 

	file.writeToPipe = writeToPipe;
	f.writeToPipe = writeToPipe;
	# readFromPipe
	file.open = fopen;
	file.close = fclose;

	# fseek ... fread ... freadlines 
	# writeRDS = saveRDS 
	# loadRDS = readRDS
	# save/load ... read/write 
	
	# writeRData = saveRData = save;
	# readRData = loadRData = load;
	
	nPr = "%nPr%";
	nCr = "%nCr%";
	
	num.den = "%frac%";
	
	num.toSci = num.toScientific;
	num.toSCI = num.toScientific;
	
	num.toNat = num.toNatural;
	num.toNAT = num.toNatural;
	
	num.toFix = num.toFixed;
	num.toFIX = num.toFixed;

	num.toEng = num.toEngineering;
	num.toENG = num.toEngineering;

	num.toContinuousFraction = num.toCFrac;

	fn.StepInto = fn.stepInto;

	fn.arg = fn.args;	

	fn.method = fn.methods;
	
	# ceiling ... ceil 
	
	
functions.inPackage = fns.inPackage = fn.inPackage;

	is.dataframe = is.data.frame;

is.Inf = is.infinite;

is.boolean = is.bool = is.logical;


str.contains = is.substring;

### THESE FUNCTIONS SEEM TO BE "mono-nuclear"
is.true = isTRUE;
is.false = isFALSE;


"%~=%" = "%eq%" = is.equal;
"%~>%" = "%ge%" = is.ge; 	
"%~<%" = "%le%" = is.le;



# fn will expand out to function 
# matrix may collapse in to "m"


# maybe property.set is "meta.set" as in meta-data ?  NAHA?<



axes = axis;
nchars = nchar;

ord = utf8ToInt;  # mb_ord ?
chr = intToUtf8;


dataframe = data.frame;

as.dataframe = as.data.frame;
ceil = ceiling;

ln = log;

len = length; 

file.delete = unlink;

is.windoze = is.windows;

invisilbe = invisible;

inode.exists = inode.exists_ = path.exists_;

str.diff = str.subtract;

str.startsWith = str.begin;
str.endsWith = str.end

dir.name = dirname;

tmp = tmpdir = temp = tempdir;



filename = basename;



"%GLOBAL%" = .GLOBAL.;
"%TO%" = .TO.;	
"%THIS%" = .THIS.;  
"%+%" = .ADD.;   # outer addition of two vectors ... 



##### INCREMENTING OPERATORS #####
"%++%" = .PLUS_PLUS.;		  
"%+=%" = .PLUS_EQUAL.;
"%--%" = .MINUS_MINUS.;		
"%-=%" = .MINUS_EQUAL.;

##### STRING OPERATORS #####
"%|=%" = .PIPE_EQUAL.;
"%=|%" = .EQUAL_PIPE.;
"%|%" = .PIPE.;
"%.%" = .DOT.;
"%. %" = .DOT_SPACE.;
"%.=%" = .DOT_EQUAL.;


# > x= "monte"
# > y = "alex"
# > z = "mama"
# > x %.% y
# [1] "monte alex"
# > x %.% y %.% z
# [1] "monte alex mama"
# > x %.% y %.% z %.% y
# [1] "monte alex mama alex"
# > zz = x %.% y %.% z %.% y
# > zz
# [1] "monte alex mama alex"
# > zz %.=% a
# > zz
# [1] "monte alex mama alex|-7"
# > zz %=.% b
# > zz
# [1] "monte alex mama alex|-76|"


 
	
"%IN%" = IN;


as.Type = as.type;
readChars = readChar;


striptags = strip.tags;
str.striptags = strip.tags;
strip_tags = strip.tags;

function.exists = check.fn;
fn.exists = check.fn;
	
obj.exists = check.obj;

# maybe just use check.type with "NULL" added logic 
# isset = is.set;


rowbind = rbind;
colbind = cbind;
deput = dput;
deput.one = dput.one;


strpos = str.pos;


quick.source = qucik = quick; 

gcd = lcd = gcd.lcd = lcm = gcd.lcm;
	
	
	
	
	
alias.add = function()
	{
	# useful for documentation ...
	
	}