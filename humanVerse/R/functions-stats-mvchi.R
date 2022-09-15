
# SEE https://github.com/MonteShaffer/R-ramblings/blob/master/R.mvchi/

## can we create rmvchi (using mvfast library)
## can we create dmvchi ... 

stats.pmvchi = function(c,nu,t,rho,alpha=1)	
	{ 
	b = sqrt(rho);	a = sqrt(1-rho);	v = b^2/a^2;	crit=c/a^2;

	(alpha - 1) + nu / (2^(nu/2)*gamma(nu/2 +1)) * 
	    (integrate(
			function(r) 
				    {
				    ((pchisq(crit,df=nu,ncp=v*r^2))^t)*r^(nu-1)*exp(-1/2 * r^2)
				    }
			, lower=0, upper=Inf)$value);
	}

pmvchi = stats.pmvchi;



stats.qmvchi = function(alpha,nu,t,rho,fudge=0.99,tol=.Machine$double.eps)	
	{ 
	b = sqrt(rho);	a = sqrt(1-rho);	v = b^2/a^2;	
	# starting values based on univariate, ncp with (r=1) using fudge scaling
		lower = (1-fudge) * qchisq(1-alpha,nu,ncp=v); 			
		upper = (1+fudge) * qchisq(1-alpha,nu,ncp=v);

	uniroot(pmvchi,c(lower,upper),nu=nu,t=t,rho=rho,alpha=alpha,tol = tol)$root;
	}
	
qmvchi = stats.qmvchi;	