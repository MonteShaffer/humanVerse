
	# NO to fractions (acceleration is unique)
	# what about FORCE / WORK ( Newton / Joule )???
	# not now ... 
	# unit.tests with other libraries???
	
	
	# https://www.nist.gov/system/files/documents/2021/03/18/ansi-nist_archived_2010_geographic.pdf
	# COORDINATES ... https://en.wikipedia.org/wiki/ISO_6709
	# lat,lon,alt ... dec, dec, meters 
	# ... have lon offset (anchor) ... xela is Khufur ... Pyramid 
	# DEG,DMS,GRT
	# GMT type format CCYYMMDDHHMMSSz
	# DEG ; Long Name: DegreeValue | Optionally populated, the format shall be ±xxx.xxxx±yyy.yyyy, where x refers to latitude and y refers to longitude. Can be auto captured/converted using GPS signals when available. For example, +039.1455- 077.2057. 
	# DMS ; Long Name : DegreeMinuteSecondValue | Optionally populated, the format shall be ±xxxDxxMxxS±yyyDyyMyyS, where x refers to latitude and y refers to longitude. Can be auto captured/converted from GPS signals when available. For example, +039D08M44S-077D12M20S.
	# 48°24′42″N 114°20′24″W # https://en.wikipedia.org/wiki/Whitefish,_Montana
	# https://geohack.toolforge.org/geohack.php?pagename=Whitefish,_Montana&params=48_24_42_N_114_20_24_W_region:US-MT_type:city
	# DMS		48° 24′ 42″ N, 114° 20′ 24″ W
	# Decimal	48.411667, -114.34
	# Geo URI	geo:48.411667,-114.34
	# https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
	# https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system#Simplified_formulae
	# These grid zones are uniform over the globe, except in two areas. On the southwest coast of Norway, grid zone 32V (9° of longitude in width) is extended further west, and grid zone 31V (3° of longitude in width) is correspondingly shrunk to cover only open water. Also, in the region around Svalbard, the four grid zones 31X (9° of longitude in width), 33X (12° of longitude in width), 35X (12° of longitude in width), and 37X (9° of longitude in width) are extended to cover what would otherwise have been covered by the seven grid zones 31X to 37X. The three grid zones 32X, 34X and 36X are not used.
	# UTM	11U 696832 5365475
	# always an acception ...
	# maybe make XTM from GIZA ... 
	# https://cran.r-project.org/web/packages/geosphere/geosphere.pdf
	#02° 31’ 21" North by 32° 5’ 39" East
	# 02 31 21 N, 32 5 39 E
	# 023121N, 320539E
	# 023121, 320539

# x could be a list of long/lati/alt  ... from="list" 
# x could be strings in a certain format ... most likely
# allow geocaching format, any format ... fixed-width?

# formatter structure?
# HDDD° MM. MMM ... N56°10.240 W003°22.260
# NT1479398699 (or NT147986 if it's only 6 digits). The 10 digit number gives an accuracy on the map to 1 meter, the 6 digit reference gives an accuracy on the map to 100 meters. For more information about the OS GB grid click here (PDF document download). 
# WSG84
# https://boulter.com/gps/#%2037%2023.516%20-122%2002.625
# Decimal Degrees (WGS84)
# Degrees, Minutes & Seconds
# Latitude	Longitude
# N37 23 30	W122 02 37
# UTM ... 10N	584646	4138781
# javascript ... https://boulter.com/gps/UTM.js
#  http://home.hiwaay.net/~taylorc/toolbox/geography/geoutm.html
# https://web.archive.org/web/20201114001919/http://home.hiwaay.net/~taylorc/toolbox/geography/geoutm.html
# https://github.com/ethz-asl/fw_qgc/blob/master/src/UTM.h
# https://github.com/ethz-asl/fw_qgc/blob/master/src/UTM.cpp 
# https://fypandroid.wordpress.com/2011/09/03/converting-utm-to-latitude-and-longitude-or-vice-versa/
# https://fypandroid.wordpress.com/2011/09/03/converting-utm-to-latitude-and-longitude-or-vice-versa/

# a = radius.equator; b = radius.poles; flattening = (a-b)/a 
geo.earth = list(
	"NAD83" = list("r.e" = 6378137, "r.p" = 6356752.3142),  # GLOBAL
	"WSG84" = list("r.e" = 6378137, "r.p" = 6356752.3142),	# GLOBAL
	"GRS80" = list("r.e" = 6378137, "r.p" = 6356752.3141),	# US 
	"WGS72" = list("r.e" = 6378135, "r.p" = 6356750.5),		# NASA/DOD
	"Australian1965" = list("r.e" = 6378160, "r.p" = 6356774.7),	# Australia
	"Krasovsky1940" = list("r.e" = 6378245, "r.p" = 6356863.0), # Soviet Union
	"Hayford1909" = list("r.e" = 6378388, "r.p" = 6356911.9), # Hayford 1909
	"Clarke1880" = list("r.e" = 6378249.1, "r.p" = 6356514.9), #Clarke 1880 (France/Africa)
	"Clarke1866" = list("r.e" = 6378206.4, "r.p" = 6356583.8), # Clarke North America
	"Airy1830" = list("r.e" = 6377563.4, "r.p" = 6356256.9),  # Great Britain
	"Bessel1841" = list("r.e" = 6377397.2, "r.p" = 6356079.0), # Central Europe/Chile
	"Everest1830" = list("r.e" = 6377276.3, "r.p" = 6356075.4) # South Asia
	);
	
# retired PROF??
# https://web.archive.org/web/20180216112824/http://www.uwgb.edu:80/dutchs/UsefulData/UTMFormulas.htm
# https://web.archive.org/web/20180223073805/http://www.uwgb.edu:80/DUTCHS/UsefulData/ConvertUTMNoOZ.HTM
# https://web.archive.org/web/20170908011409/http://www.uwgb.edu/dutchs/UsefulData/UTMConversion2015m.xls
# https://geographiclib.sourceforge.io/
# https://geographiclib.sourceforge.io/C++/doc/index.html
# https://geographiclib.sourceforge.io/JavaScript/doc/geographiclib-dms_src_DMS.js.html#line509
# https://geographiclib.sourceforge.io/JavaScript/doc/tutorial-3-examples.html

	
	
geo.init = function() 
	{
	geo.earth = list(
	"NAD83" = list("r.e" = 6378137, "r.p" = 6356752.3142),  # GLOBAL
	"WSG84" = list("r.e" = 6378137, "r.p" = 6356752.3142),	# GLOBAL
	"GRS80" = list("r.e" = 6378137, "r.p" = 6356752.3141),	# US 
	"WGS72" = list("r.e" = 6378135, "r.p" = 6356750.5),		# NASA/DOD
	"Australian:1965" = list("r.e" = 6378160, "r.p" = 6356774.7),	# Australia
	"Krasovsky:1940" = list("r.e" = 6378245, "r.p" = 6356863.0), # Soviet Union
	"Hayford:1909" = list("r.e" = 6378388, "r.p" = 6356911.9), # Hayford 1909
	"Clarke:1880" = list("r.e" = 6378249.1, "r.p" = 6356514.9), #Clarke 1880 (France/Africa)
	"Clarke:1866" = list("r.e" = 6378206.4, "r.p" = 6356583.8), # Clarke North America
	"Airy:1830" = list("r.e" = 6377563.4, "r.p" = 6356256.9),  # Great Britain
	"Bessel:1841" = list("r.e" = 6377397.2, "r.p" = 6356079.0), # Central Europe/Chile
	"Everest:1830" = list("r.e" = 6377276.3, "r.p" = 6356075.4) # South Asia
	);

	print(names(geo.earth));
	}


geo.setModel = function(model = "WSG84")
	{
	
	
	
	}

geo.dist = function(lat, lon, model="WSG84")
	{
	
	
	
	}
	
# 	the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski". Any unambiguous substring can be given.

# # how many miles is 1 degree of latitude
# latitude.factor = 69;  # rough mile estimate  # 68.703 ?
# # how many miles is 1 degree of longitude
# longitude.factor = 54.6;  # rough mile estimate  
# delta.latitude = radius.miles / 68.703 ;
#    delta.longitude = radius.miles / (69.172 * cos(deg2rad(my.longitude))); 
# dist(chicago, method="manhattan", diag=FALSE, upper=TRUE);
# rows are [places], cols are x,y,z...

# jarowinkler(w1.singular, w1);

dd2dms = function()
	{
	//Input= xd(long) and yd(lat)
	
	}
	
dms2dd = function(D, M, S)
	{
	# //Input = xdd xm xs (long) and ydd ym ys (lat)	
		
	}
	
geo.convert = function(..., from="", to="", model="WSG84")
	{
	
	
	
	}
