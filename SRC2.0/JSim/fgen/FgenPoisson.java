/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Poisson-like distribution

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;


public class FgenPoisson extends FgenSlave {
	double trep;
	FgenInterp storage;

	// constructor
	public FgenPoisson(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { return 1; }

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
	    RegularGridData grid = domainGrid(0);
	    double tstart=grid.min();
	    double tend  =grid.max();
	    double tdelta=grid.delta();
	    double are = realVal("area");
            double tmean  = realVal("tMean");
            double reldis = realVal("RD");
	    double frpeak = realVal("frPeak");
	    storage = new FgenInterp(tstart, tend, tdelta);
            poisson( storage, are, tmean, reldis, frpeak);
            trep = realVal("timeToRepeat");                                                                     
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

	// distribution calculation
	public static void poisson ( FgenInterp storage,
                              double area, double tmean,
                              double reldis, double frPeak) {
            double tstart=storage.t[0];
            double tend=storage.t[storage.length-1];
            double dt;
            if(storage.length>1) {dt=storage.t[1]-storage.t[0];}
            else {dt=0.0;}
            if(tend<=tstart || storage.length <2 || dt<=0 || tmean< tstart) return;
	    double tshift = 0-tstart;
            double tstarts= tstart+tshift;
            double tmeans = tmean+tshift;
            double tends  = tend+tshift;

	    double tbar=tmeans;
	    double rd = Math.max(Math.min(1,reldis),0.01);
	    int nlag = (int) (1./(rd*rd)+0.5);
	    double xlag = (double) nlag;
	    boolean done=false;
	    if(tbar==0) {
	        storage.ft[0]=1;
                done=true;
	    }
	    // SPIKE CONDITION
	    if( !done &&  nlag >= 16*tbar*tbar/(dt*dt) ) {
	        int ih = Math.min((int)((tbar)/dt +0.5 ),storage.length-1);
	        storage.ft[ih]=1.0;
	        done=true;
	    }
	    if(!done) {
	        double powlim = Math.pow(1e307,1/xlag);
	        double aconst=xlag/tbar;
	        double temp = aconst*dt;
		double aconst2 = Math.exp(-dt/tbar);
	        double expon=1.0;
	        double n = xlag-1.0;
	        double xvar=0.0;
	        if(n<=0) xvar=1.0;
                
                boolean frmax=false;
                double frpeak = Math.min(0.1,Math.max(1e-10,frPeak));
	        double maxPeak = 0;
	        for (int i=0; i<storage.length && !frmax;  i++) {
	            if (expon*xvar<powlim) {
	                storage.ft[i]=aconst*Math.pow(expon*xvar,n);
	            }
	            else {
	                storage.ft[i]=0;
	            }
	            xvar=xvar+temp;
	            aconst=aconst*aconst2;
	            expon=expon*aconst2;
	            if(storage.ft[i] < frpeak*maxPeak) {frmax=true;}
	            else {
	                maxPeak=Math.max(maxPeak,storage.ft[i]);
	            }
	            
	        }
	    }
	    storage.normalize(area);
	}
}


