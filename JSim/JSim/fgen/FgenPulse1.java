/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Single Pulse function

package JSim.fgen;

import JSim.util.*;


public class FgenPulse1 extends FgenSlave {
       	double strt;
	double dur;
	double amp;
	double off;
        double trep;

	// constructor
	public FgenPulse1(FgenMaster f, String n, FgenContext ctxt) 
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
            trep = realVal("timeToRepeat");
	}

	public double realVal(double[] vals) throws Xcept {
	    double returnValue=0;
	    double t = vals[0];
            if(trep>0) {
		t=t%trep;
                if(t<0) t=t+trep;
	    }
	    if(t>=strt && t<strt+dur) {
		return amp+off;
	    } else {
		return off;
	    }
	}
}
