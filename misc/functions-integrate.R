



computeXiFromResolution = function(x.domain, i.lim = c(0,1), 
                                    dxi = 0.01, forceEven = TRUE, oxi = 10)
  {
  # dxi is "eps" like ... resolution per unit of x
  # if dxi = 100, that means 100 eps per unit of x
  # if dxi = .01, that means eps is about 0.01 in x units, and we compute eps from that ... 
  # "eps" is like "steps"
  # x.domain is the larger picture, i.lim is the integral boundaries ...
  # oxi = 10 ... 10 times fewer elements in the x.domain than in the actual limiting area
  # if (oxi = 1), it will be the same dxi resolution 
  
  # crude.lower ... if(x.domain[1] < i.lim[1])
  # fine.integral ... 
  # crude.upper
  
  if(is.null(i.lim)) { i.lim = c(0,1); }
  
  crude.lower = c();
  if(x.domain[1] < i.lim[1])
    {
    x.lower = x.domain[1];
    x.upper = i.lim[1];
    x.range = abs(x.lower - x.upper);
    
    if(dxi < 1)  # a fraction means steps
      {
      crude.lower  = seq( from = x.lower, 
                            to = x.upper, 
                            by = dxi * oxi);
      } else  {
              crude.lower  = seq( from = x.lower, 
                                    to = x.upper, 
                            length.out = x.range * dxi / oxi);
              }  
    }
  
  
  crude.upper = c();
  if(x.domain[2] > i.lim[2])
    {
    x.upper = x.domain[2];
    x.lower = i.lim[2];
    x.range = abs(x.lower - x.upper);
    
    if(dxi < 1)  # a fraction means steps
      {
      crude.upper  = seq( from = x.lower, 
                            to = x.upper, 
                            by = dxi * oxi);
      } else  {
              crude.upper  = seq( from = x.lower, 
                                    to = x.upper, 
                            length.out = x.range * dxi / oxi);
              }  
    }

  
  
  fine.limit = c();
    x.lower = i.lim[1];
    x.upper = i.lim[2];
    x.range = abs(x.lower - x.upper);
    
    if(dxi < 1)  # a fraction means steps
      {
      fine.limit  = seq( from = x.lower, 
                            to = x.upper, 
                            by = dxi);
      } else  {
              fine.limit  = seq( from = x.lower, 
                                    to = x.upper, 
                            length.out = x.range * dxi);
              }  

  if(forceEven == TRUE)
    {
    fine.len = length(fine.limit);
    if(fine.len %% 2 == 1) 
      { 
      fine.len = 1 + fine.len;
      # update it
      fine.limit  = seq( from = x.lower, 
                           to = x.upper, 
                   length.out = fine.len);
      }
    }
  
  
  xi = unique( c( crude.lower, fine.limit, crude.upper ) );
  
  list( "lower"    = (crude.lower),
        "integral" = (fine.limit),
        "upper"    = (crude.upper),
        "xi"       = (xi)
      );
  
  }


parseNumericalFunctionString = function(fstr="normal: -1, 1")
  {
  ostr = fstr; # original
  fstr = trimMe(fstr);
  ########### normal   -3.5 , 3.5
  #s = str_split( fstr ,":")[[1]];
  s = strsplit( fstr ,":", fixed=TRUE)[[1]];
  
    fkey = tolower( trimMe(s[1]) );
    fkey.3 = substr(fkey,1,3);
  #ss = str_split( trimMe(s[2]),">")[[1]];
  ss = strsplit( trimMe(s[2]),">", fixed=TRUE)[[1]];
  
  #xd = str_split( trimMe(ss[1]),",")[[1]];
  xd = strsplit( trimMe(ss[1]),",", fixed=TRUE)[[1]];
  xd = trimMe(xd);
    fdomain.x = suppressWarnings( as.numeric(xd) );
    
  xp = trimMe(ss[2]);  
  
  list( "fkey.3"  = fkey.3, "fkey" = fkey, "fstr" = ostr,
        "fparams" = xp,     "fdomain.x" = fdomain.x);
  }


buildNumericalDataForIntegral = function(fprep="normal: -1, 1", 
                                         i.lim = NULL, dxi = 0.01, forceEven = TRUE)
  {
  if(is.character(fprep))
    {
    fstr = fprep;
    fprep = parseNumericalFunctionString(fstr);
    }
  
  if(!is.list(fprep))
    {
    stop("Something is wrong, fprep is not a list");
    }
  
  fkey.3       = fprep$fkey.3;
  fdomain.x    = fprep$fdomain.x;
  if(is.null(i.lim)) { i.lim = fdomain.x; } # default for i.lim
  
  xp           = fprep$fparams;
  
###############  CASE :: normal  ###############  
  if(fkey.3 == "nor")
    {
    ### default domain ### 
    x.domain = c(-4, 4);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
     
    ### default params ### 
    mean = 0; 
    sd = 1; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dnorm(xi, mean=mean, sd=sd);
     # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
    }

###############  CASE :: t  ###############  
  if(fkey.3 == "t")
    {
    ### default domain ### 
    x.domain = c(-4, 4);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    df = 1; 
     # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dt(xi, df=df);
     # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
  }

###############  CASE :: Hotelling's T2 distribution  ###############  
  if(fkey.3 == "t^2" || fkey.3 == "t2" || fkey.3 == "hot")
    {
    ### default domain ### 
    x.domain = c(0.01, 16);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain <=0 ) { stop("t^2 requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    p = 4;  # number of features
    n = 50;  # sample size  # library(Hotelling);
    rev.x = FALSE; 
     # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    df1 = p; df2 = n-p+1;
    yi = df(xi, df1=df1, df2=df2);
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
     # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
  }  
  
###############  CASE :: f  ###############  
  if(fkey.3 == "f")
    {
    ### default domain ### 
    x.domain = c(0.01, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain[1] <= 0 ) { stop("F requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    df1 = 5; 
    df2 = 2; 
    rev.x = FALSE; 
     # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = df(xi, df1=df1, df2=df2);
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # if(rev.x == TRUE) { xi = rev(xi); yi = rev(yi); }
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
  }    
  
###############  CASE :: chisq  ###############     
  if(fkey.3 == "chi")
    {
    ### default domain ### 
    x.domain = c(0.01, 8);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain[1] <=0 ) { stop("chi^2 requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    df = 2; 
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dchisq(xi, df=df);
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
    }
 
###############  CASE :: gamma  ###############     
  if(fkey.3 == "gam")
    {
    ### default domain ### 
    x.domain = c(0.01, 16);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    if(x.domain[1] <=0 ) { stop("gamma requires x > 0 for domain"); }
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    shape = 1; 
    scale = 2; 
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = dgamma(xi, shape=shape, scale=scale);
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
    }

###############  CASE :: quadratic  ###############     
  if(fkey.3 == "qua" || fkey.3 == "par")
    {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    a = 1;
    h = 0;
    k = 0;
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = a * (xi - h)^2 + k;
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
    }  

###############  CASE :: exponential  ###############     
  if(fkey.3 == "exp")
    {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    a = 1;
    b = -2;
    h = 0;
    k = 0;
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = a * exp(b*(xi - h)) + k;
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
    }    
    
  
  
  
  ###############  CASE :: brachistochrone ###############     
  if(fkey.3 == "bra" || fkey.3 == "cos")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 1;
      b = -2;
      h = 0;
      k = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      
      yi = a * cosh(b*(xi - h)) + k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
      return ( list("x" = xi, "y" = yi) );
    }    
    
  }
  
  
  ###############  CASE :: power  ###############     
  if(fkey.3 == "pow")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 2;
      h = 0.3;
      p = 3;
      k = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      yi= a*(xi+h)^p + k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
      return ( list("x" = xi, "y" = yi) );
    }    
    
  }
  
  
  ###############  CASE :: log  ###############     
  if(fkey.3 == "log")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 2;
      h = 0.3;
      k = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      yi=a*log(xi+h) + k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
      return ( list("x" = xi, "y" = yi) );
    }    
    
  }
  
  ###############  CASE :: bump  ###############     
  if(fkey.3 == "bum")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a = 2;
      b = 1;
      c = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      smoothbump=function(x,b=1,c=2){ifelse( (b*x+c)^2>=1,0,exp(1/ (( b*x + c)^2 - 1))  )
      }
      yi=a*smoothbump(xi,b,c);
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
      return ( list("x" = xi, "y" = yi) );
    }    
    
  }
  
  
  ###############  CASE :: wavelet  ###############     
  if(fkey.3 == "wav")
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      A = 2;
      a=1;
      b = 1;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      sinc=function(x,a=-7,b=-6){
        
        y=ifelse(x==0,1, (sin( a*x + b ))/(a*x+b) )
        return(y)
      }
      
      yi=A*sinc(xi,a,b);
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
      return ( list("x" = xi, "y" = yi) );
    }    
    
  }
  
  ###############  CASE :: sinusoid ###############     
  if(fkey.3 == "sin" || fkey.3 == "cos" )
  {
    {
      ### default domain ### 
      x.domain = c(-5, 5);
      if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
      # figure out resolution with dxi and forceEven, given x domain  
      # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
      i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
      # resolution is at a generic unit scale, not specific to the integral domain
      
      ### default params ### 
      a=1;
      b = 1;
      c=2;
      k=0;
      rev.x = FALSE; 
      # could be overwritten with eval(parse
      eval(parse(text = xp));
      
      yi=a*sin(b*xi+c)+k;
      
      if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
      # plot(xi,yi);
      return ( list("x" = xi, "y" = yi) );
    }    
    
  }
  
###############  CASE :: uniform  ###############      
  if(fkey.3 == "uni" || fkey.3 == "rec")
    {
    ### default domain ### 
    x.domain = c(0,1);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
      xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
    
    ### default params ### 
    height = 1;
    eval(parse(text = xp));
    yi = 0*xi + height;
    #plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
    }
  
  ###############  CASE :: triangle  ###############     
  if(fkey.3 == "tri" )
  {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
     
    ### default params ### 
    height = 1;
    rev.x = FALSE; 
    # could be overwritten with eval(parse
    eval(parse(text = xp));
    
    yi = height*(1-abs(xi));
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
  }  
  

    # can we do a generic one with x,y
    # y = ax + b ... let's allow any values, no defaults?
  
  ###############  CASE :: function  ###############     
  if(fkey.3 == "fun" )
  {
    ### default domain ### 
    x.domain = c(-5, 5);
    if(length(fdomain.x) == 2) { x.domain = fdomain.x;}
    # figure out resolution with dxi and forceEven, given x domain  
    # xi = computeLengthFromResolution(x.domain, dxi = dxi, forceEven=forceEven); 
    i.xi = computeXiFromResolution(x.domain, i.lim = i.lim, dxi = dxi, forceEven=forceEven); 
    xi   = i.xi$xi;
    # resolution is at a generic unit scale, not specific to the integral domain
     
    ### default params ### 
    # height = 1;
    # rev.x = FALSE;
    # could be overwritten with eval(parse
    # eval(parse(text = xp));
    # 
    # yi = height*(1-abs(xi));
    
    if(rev.x == TRUE) { xi = rev(xi); } # switches skew-left to skew-right
    # plot(xi,yi);
    return ( list("x" = xi, "y" = yi) );
  }  
  
##### END OF THE ROAD #####    
  warning("fkey.3 was not found, returning NA");
  return (NA);  
  }





differentSigns = function(a,b)
  {
  if(Re(a) > 0 && Re(b) < 0) { return(TRUE); }
  if(Re(a) < 0 && Re(b) > 0) { return(TRUE); }
  return(FALSE);
  }

computeNumericalIntegration = function(info,
    method="support",
    FUN="yi*1",
    tol= (.Machine$double.eps ^ 0.25),
    skip.signs = FALSE,
    fparams=NULL,
    stop=NULL,
    verbose=FALSE,
    animatePolygons = NULL, # if not null, this is Sys.sleep(3)
    showPolygons = FALSE, # requires an active plot
    polygon.lwd = 1,
    polygon.border = NA,  # set color for each polygon border
    polygon.col.pos = "green", # positive-area color
    polygon.col.neg = "red", # negative-area color
    return="result"
    )
  {
  if(method=="support")
    {
    ysupport = info$data$f.x;
    xdomain = info$i.xi$integral;
      xlen = length(xdomain);
      xskip = length(info$i.xi$lower);
    yfinal = ysupport[(xskip+1): (xskip+xlen)];
    }
  if(method == "string")
    {
    xdomain = info$x;
    yfinal = info$y;
      xlen = length(xdomain);
    # could be reversed
    if(xdomain[1] > xdomain[xlen])
      {
      print("REVERSED");
      xdomain = rev(info$x);
      yfinal = rev(info$y);
      }
    }
  
  sign.changes = 0;
  ## track exponential order ... when did it meet a tolerance (x)
  ## if not, what was its avergae area (y) for the last (10) observations
  ## push/pop ... https://stackoverflow.com/questions/28687806/a-better-way-to-push-and-pop-to-from-lists-in-r
  
  # http://adv-r.had.co.nz/Environments.html
  # parent.env()
  extractList = function(myList, envir = .GlobalEnv)
    {
    n.myList = length(myList);  # maybe create an extract function ... 
                                  # parent.env() ... 
    if(n.myList > 0)
      {
      for(i in 1:n.myList)
        {
        assign(names(myList)[i], myList[[i]], envir = .GlobalEnv);
        }
      }
    }
  getY = function(xi=1,yi=1,FUN="1",fparams=NULL)
    {
    #y.env = new.env();
    #extractList(fparams, envir = y.env);
    extractList(fparams);
    # FUN "mean(exp((tc*xi)^2*varvec/2)*cos(tc*xi*z))"
    #yi * eval(parse(text = FUN)); 
    
    eval(parse(text = FUN));
    }
##### let's break down eps further if there is a sign change ##### 
  eps.a = c();
  
  if(skip.signs)
  {
  final.x = xdomain;
  final.y = yfinal;
  } else {
  
  final.x = c();
  final.y = c();
  for(i in 1:(xlen-1))
    {
    # maybe loop through once and get the xc,yc for sign changes ... 
    x = xdomain[i];
    xn = xdomain[i+1]; # next
    
    
    
    ## we can do EVAL on FUN here ???
                                      # FUN "mean(exp((tc*xi)^2*varvec/2)*cos(tc*xi*z))"
    y = yfinal[i];                    # eval(parse(text = xp));
    yn = yfinal[i+1]; # next
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    # we use getY to build "betweeners, but let's not save as such.
    # the double getY was creating the exponential decay.
    
    y.e = getY( xi = x,  yi = yfinal[i],  FUN,fparams);
    yn.e = getY(xi = xn, yi = yfinal[i+1],FUN,fparams);
    
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    final.x = c(final.x,x);
    final.y = c(final.y,y);
    
    if(differentSigns(y.e,yn.e))
      {
      yc = 0;
      #xc = x + (xn - x) * (yc - y) / (yn - y);
      xc = x + (xn - x) * (yc - y.e) / (yn.e - y.e);
      
      final.x = c(final.x,xc);
      final.y = c(final.y,yc);
      
      sign.changes = 1 + sign.changes;
      }
    }
    final.x = c(final.x,xn);
    final.y = c(final.y,yn);
        }
  
  xlen = length(final.x);  
##### let's loop and calculate numerical area #####    
  result = list();
  result$positive = result$negative = result$absolute = result$total = 0;
  polygons = list();
  for(i in 1:(xlen-1))
    {
    x = final.x[i];
    
    if(!is.null(stop))
      {
      if(stop < x) { break; }
      }
    
    xn = final.x[i+1]; # next
    
    
    y = final.y[i];
    yn = final.y[i+1]; # next
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    ## we can do EVAL on FUN here ???
    y = getY( xi = x,  yi = final.y[i],  FUN,fparams);
    yn = getY(xi = xn, yi = final.y[i+1],FUN,fparams);
    
    
    #print(paste0("y: ",y," --> yn: ",yn));
    
    if(verbose)
    {
    print(paste0(" ########### i = ",i,"  ###########"));
    print(paste0("y: ",y," --> yn: ",yn));
    print(paste0("x: ",x," --> xn: ",xn));
    }
    
    
    eps = abs(xn - x) * (y + yn) / 2;  
    
    eps.a = c(eps.a, eps);
     
    
    
    # if(is.na(eps)){eps=0}
    result$total = result$total + eps;
    result$absolute = result$absolute + abs(eps);
    if(Re(eps) > 0)
      {
      mycol = polygon.col.pos;
      result$positive = result$positive + eps;
      }
    if(Re(eps) < 0)
      {
      mycol = polygon.col.neg;
      result$negative = result$negative + eps;
      }
    
    # polygons

      px = c(x, x, xn, xn);
      py = c(0, y, yn,  0);
     
      if(showPolygons)
        {
        polygon(px, py, col=mycol, border=polygon.border, lty=1, lwd=polygon.lwd);
        }


    if(verbose)
    {
    print(paste0("x: ",x, "  ==> eps: ",eps, " ... ", result$total));
    }
    
    
    polygons[[i]] = list("x" = px, "y" = py);
    
#### end stop ??? ####      
    
      }
     

  result$sign.changes = sign.changes;
  result$eps = eps.a;
  
  # print(paste0("C.t = ",result$total));
  # print(paste0("F.s = ",result$total));
  # print(paste0("Area = ",result$total));
  
  
  if(return=="result") { return(result); }
  if(return=="total") { return(result$total); }
  if(return=="abs" || return == "absolute") { return(result$absolute); }
  
  if(return=="all") { return(list("result"=result,"polygons"=polygons)); }
  
  
  }
