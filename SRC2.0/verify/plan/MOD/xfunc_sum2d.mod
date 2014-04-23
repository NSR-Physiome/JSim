// xfunc: 2D summation

source real function dosum(u@x@y) {
	language="java";
	reentrant="true";
	maincode={{
	    double tot = 0;
            int n = u.nsamples();
	    for (int i=0; i<n; i++) 
	        tot += u.realVal(i);
	    return tot;	
	}};
}

math main {
	realDomain x, y;
	x.min=1; x.max=3; x.delta=1;
	y.min=1; y.max=2; y.delta=1;
	real u(x,y) = x+y;
	real w = dosum(u@x@y);
}

