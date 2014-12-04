/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// random-walk distribution

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;


public class FgenRandomWalk extends FgenSlave {
	double trep;
	FgenInterp storage;

	// constructor
	public FgenRandomWalk(FgenMaster f, String n, FgenContext ctxt) 
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
            ranwlk( storage, are, tmean, reldis, skew, frpeak);
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
	public static void ranwlk  ( FgenInterp storage,
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
	    double tbar=0;
	    double skew= Math.max(skewn,3.001*reldis);
	    double tkappa = skew*Math.pow(2.,0.5)/3.0;
	    if(skew>0) {tbar=3*reldis*tmeans/skew;}
	    double ta = tmeans-tbar;
	    if(tkappa<=0 || tbar <=0 || ta<0 ) return;
	    boolean done=false;
	    // SPIKE CONDITION
	    if( dt >= 4*tbar*tkappa/Math.pow(2.0,0.5) ) {
	        int ih = (int) ( (ta+tbar)/dt +0.5 );
	        storage.ft[ih]=1.0;
	        done=true;
	    }
	    if(!done) {
	        storage.ft[0]=0.0;
	        double scale=1.0/(Math.pow(Math.PI,0.5)*tbar); 

                boolean frmax=false;
                double frpeak = Math.min(0.1,Math.max(1e-10,frPeak));
	        double maxPeak = 0;
	        for (int i=1; i<storage.length && !frmax;  i++) {
		    double tau = (storage.t[i]-tstart -ta)/tbar;
	            if (tau>0) {
	                double rksqt=1./(tkappa*Math.pow(tau,0.5));
	                double coeff= rksqt*scale/tau;
	                double v = (1.0-tau)*rksqt;
	                storage.ft[i]=coeff*FgenDistrib.expf(-v*v);
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


