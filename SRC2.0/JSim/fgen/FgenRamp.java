/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Ramp function

package JSim.fgen;

import JSim.util.*;


public class FgenRamp extends FgenSlave {
        double strt;
	double dur;
	double amp;
	double off;
        double expo;
        double trep;
        double tend;

	// constructor
	public FgenRamp(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { return 1; }

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
            strt = realVal("startTime");
	    dur  = realVal("duration");
	    amp  = realVal("amplitude");
	    off  = realVal("offset");
            expo = realVal("exponent");
            trep = realVal("timeToRepeat");
            tend = strt+dur;
	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    double t = vals[0];
            if(trep>0) {
		t=t%trep;
                if(t<0) t=t+trep;
	    }
            if(t>strt && t<=strt+dur) {
		return Math.pow((t-strt)/dur,expo)*amp+off;
	    } else {
		return 0;
	    }
	}

}


