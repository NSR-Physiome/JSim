/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Gaussian distribution

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;


public class FgenGauss extends FgenSlave {
	double trep;
	FgenInterp storage;

	// constructor
	public FgenGauss(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { return 1; }

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
            RegularGridData t=domainGrid(0);
            double tstart=t.min();
            double tend  =t.max();
            double tdelta=t.delta();
	    double are = realVal("area");
            double tmean  = realVal("tMean");
            double reldis = realVal("RD");
	    double frpeak = realVal("frPeak");
	    trep = realVal("timeToRepeat");
	    storage = new FgenInterp(tstart, tend, tdelta);
            FgenDistrib.gaudis( storage, are, tmean, reldis, frpeak);
 	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    double tnow = vals[0];
            if(trep>0) {
                tnow=tnow%trep;
                if(tnow<0) tnow=tnow+trep;
            }
	    return storage.valueAt(tnow);
	}
}
