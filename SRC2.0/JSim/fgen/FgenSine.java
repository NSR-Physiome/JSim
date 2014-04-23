/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//  Sine     function

package JSim.fgen;

import JSim.util.*;


public class FgenSine extends FgenSlave {
        double strt;
	double dur;
	double amp;
	double off;
        double Ph;
	double per;
        double trep;

	// constructor
	public FgenSine(FgenMaster f, String n, FgenContext ctxt) 
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
            Ph   = realVal("phase");
	    per = realVal("period");
            if(per<=0) per=1;
            trep = Math.abs(realVal("timeToRepeat"));
	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    double returnValue=0;
	    double t = vals[0];


            double P    = (-1.*Ph)%1.0;
            if(P<0) P=P+1.0;

            double freq=2*Math.PI/per   ;

            if(trep<=0) {t=t;}
	    else {
		t=t%trep;
                if(t<0) t=t+trep;
	    }
	    if(t>=strt && t<=strt+dur) {
//              Adjust for phase
                t=t+P*per   ;
                returnValue=off+amp*Math.sin(freq*(t-strt));
	    }
	    else {
		returnValue = 0;
	    }
	    return returnValue;
	}

}


