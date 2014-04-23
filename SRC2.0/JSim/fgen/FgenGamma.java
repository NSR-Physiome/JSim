/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// gamma-variate distribution

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;


public class FgenGamma extends FgenSlave {
	double trep;
	FgenInterp storage;

	// constructor
	public FgenGamma(FgenMaster f, String n, FgenContext ctxt) 
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
            double skew   = realVal("skewn");
	    double frpeak = realVal("frPeak");
	    storage = new FgenInterp(tstart, tend, tdelta);
            gamvar( storage, are, tmean, reldis, skew, frpeak);
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
	public static void gamvar ( FgenInterp storage,
                              double area, double tmean,
                              double reldis, double skewn, double frPeak) {
            double tstart=storage.t[0];
            double tend=storage.t[storage.length-1];
            double dt;
            if(storage.length>1) {dt=storage.t[1]-storage.t[0];}
            else {dt=0.0;}
            if(tend<=tstart || storage.length <2 || dt<=0 || tmean<= tstart) return;
	    double tshift = 0-tstart;
            double tstarts= tstart+tshift;
            double tmeans = tmean+tshift;
            double tends  = tend+tshift;
	    double skew=Math.max(Math.min(1.999,skewn),0.267);
	    double rd  = Math.min(reldis,skew/2.01);
	    double alpha = 4/(skew*skew) -1.0;
	    double beta = tmeans*rd*skew/2.;
	    double ta   = tmean-beta*(alpha+1);
	    if(alpha<=0 || ta<=tstart ) return;
	    boolean done=false;
	    // SPIKE ?
	    if ( 0.25*dt > beta*Math.pow(1.+alpha,0.5)) {
	        int nh = Math.min( (int) ((ta+beta*(1.+alpha))/dt+0.5), storage.length-1);
	        storage.ft[nh]=1.0;
		done=true;
	    }
	    if(!done) {
	        double tlimalpha = Math.pow(1.0e307,1/alpha);
                boolean frmax=false;
                double frpeak = Math.min(0.1,Math.max(1e-10,frPeak));
	        double maxPeak = 0;
	        for (int i=0; i<storage.length && !frmax;  i++) {
	            if( storage.t[i]<ta ) {storage.ft[i]=0;}
	            else { 
	                double t = (storage.t[i])-ta;
		        if(t<tlimalpha && t/beta<350) {storage.ft[i]=Math.pow(t,alpha)*Math.exp(-t/beta);}
	                else {storage.ft[i]=0.0; }
	            }
	            if(storage.ft[i] < frpeak*maxPeak) {frmax=true;}
	            else {
	                maxPeak=Math.max(maxPeak,storage.ft[i]);
	            }
	        }
	    }
	    storage.normalize(area);
	}
}


