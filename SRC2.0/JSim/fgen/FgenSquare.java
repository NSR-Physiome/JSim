/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  Square wave function

package JSim.fgen;

import JSim.util.*;


public class FgenSquare extends FgenSlave {
        double strt;
	double dur;
	double amp;
	double off;
	double sf;
        double ph;
	double per;
        double trep;

	// constructor
	public FgenSquare(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { return 1; }

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
            strt = realVal("startTime");
	    dur  = Math.abs(realVal("duration"));
	    amp  = realVal("amplitude");
	    off  = realVal("offset");
	    sf = Math.min(1.,Math.max(0.,realVal("shapeFactor")));
            ph   = realVal("phase");
	    per = realVal("period");
            if(per<=0) per=1;
            trep = Math.abs(realVal("timeToRepeat"));
	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    double returnValue=0;
	    double t = vals[0];
            double freq = 1.0/per   ;
            double p    = (-1.*ph)%1.0;
            if(p<0) p=p+1.0;
            if(trep<=0) {t=t;}
	    else {
		t=t%trep;
                if(t<0) t=t+trep;
	    }
            double cyoff = freq*(-strt)-p;
            if(t>=strt && t<strt+dur) {
                double xprime=(freq*(t)+cyoff)%1;
                if (xprime<0) {
                    xprime=xprime+1;}
                if (xprime<=sf) {
                    returnValue=off+amp;}
                else {
                    returnValue=off;}
             }
             else {
                    returnValue=0;
             }
	    return returnValue;
	}

}


