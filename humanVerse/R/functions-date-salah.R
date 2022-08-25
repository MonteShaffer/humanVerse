
# maybe roll into one function 
# ctype is calendar type of the date ... 
# "british" is 1752 adoption, "papal" is 1572 adoption, "julian" is 

astro.EOT = function()
	{
	# equation of time ... difference between 00:00:00 clock and sundial time 
	
	}
	
astro.DOS = function()
	{
	# declination of sunrise
	
	}
	
# generalized sunrise/sunset calculations ( atmosphere.offset = FALSE )
# humanVerse 'caveman' would not have known about defraction 
# first light as in LOTR Helm's Deep
# last light as in Big Sur sunset or Pirates:End of the World 

date.salah = function(date='2020-02-20', ctype="british"
								lat = 30, 
								long = 30, 
								alt = 0, 
								which.long="GM"
					)
	{
	
	}

# which.long = "GM", "GP" ... greenwhich meridian, ghiza pyramid
# allows for ancient longitude maps 
# based on "ecliptic"
salah.Dhuhr = function(date='2020-02-20', lat = 30, long = 30, alt = 0, which.long="GM")
	{
	# this is solar noon 
	
	}
	
salah.Chorok = function()
	{
	# prayer before sunrise ...  [twilight]
	# T.Dhur - T(0.833)
	}
	

salah.Maghreb = function()
	{
	# prayer after sunset 
	# T.Dhur + T(0.833)
	}
	
salah.Fajr = function()
	{
	# T.Dhur - T(18)
	}
	
salah.Isha = function()
	{
	# T.Dhur + T(17)
	}

salah.Asr = function()
	{
	A.n = function(n)
		{
		# arccot(n + tan(phi - D) ); # what is D?
		}
	# T.Dhur + T(  A(n)   )
	}