/*NSRCOPYRIGHT
    Copyright (C) 1999-2010 University of Washington
    Developed by the National Simulation Resource
    Department of Bioengineering,  Box 355061
    University of Washington, Seattle, WA 98195-5061.
    Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Longtail function - updated Mar 2010 GR

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;

import Jama.*;

public class FgenLongtail extends FgenSlave {
    double trep;
    FgenInterp storage;

    // constructor
    public FgenLongtail(FgenMaster f, String n, FgenContext ctxt) 
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

        // parametrs for LONGTAIL
        double pdf      = realVal("PDF"); // choose LNDc,Gauss,Poisson,Rwalk,GamVar
        double are      = realVal("area");
        double tmean    = realVal("tMean");
        double reldis   = realVal("RD");
        double skew     = realVal("skewn");
        double frpeak   = realVal("frPeak");
        double upslope  = realVal("upslope");

        double tORfr    = realVal("tORfr"); // join at tJoin or when at a % of peak
        double tJoin    = realVal("tJoin");
        double frJoin   = realVal("frJoin");

        double expORpow = realVal("expORpow"); // exponentials or power functions
        double nExp     = realVal("nExp");     // number of exponentials 1,2,3, or 4
        double w1       = realVal("w1"); 
        double w2       = realVal("w2"); 
        double w3       = realVal("w3"); 
        double w4       = realVal("w4");
        double k1       = realVal("k1"); 
        double k2       = realVal("k2"); 
        double k3       = realVal("k3"); 
        double k4       = realVal("k4"); 

        double nPow     = realVal("nPow");  // number of power functions 1 or 2
        double wpow1    = realVal("wpow1");
        double wpow2    = realVal("wpow2");
        double wpow3    = realVal("wpow3");
        double wpow4    = realVal("wpow4");
        double beta1    = realVal("beta1"); 
        double beta2    = realVal("beta2");
        double beta3    = realVal("beta3");
        double beta4    = realVal("beta4");
        
        int up = intVal("upslope");
        boolean ix = (up != 0);
        storage = new FgenInterp(tstart, tend, tdelta);
            longtail( storage, pdf, are, tmean, reldis, skew, ix, frpeak,
                tORfr, tJoin, frJoin, expORpow, nExp, w1, w2, w3, w4, 
                k1, k2, k3,k4, nPow, wpow1, wpow2, wpow3, wpow4, 
                beta1, beta2, beta3, beta4    );
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
        public static void longtail ( FgenInterp storage,
            double pdf,
            double area, double tmean,
            double rd,
            double skewn, boolean upslope, double frpeak,
            double tORfr, double tJoin, double frJoin,
            double expORpow, double nExp,
            double w1, double w2, double w3, double w4,
            double k1, double k2, double k3, double k4,
            double nPow,  double wpow1, double wpow2, 
            double wpow3, double wpow4, 
            double beta1, double beta2,
            double beta3, double beta4 ) {
                double tstart=storage.t[0];
                double tend=storage.t[storage.length-1];
                double dt;
                if(storage.length>1) 
		    dt=storage.t[1]-storage.t[0];
                else {dt=0.0;}
                if(tend<=tstart || storage.length <2 || dt<=0 || tmean<= tstart) return;
                if(pdf==0) 
		     FgenDistrib.lagdis    (storage, area, tmean, rd, skewn, upslope, frpeak);
                if(pdf==1) 
		     FgenDistrib.gaudis    (storage, area, tmean, rd, frpeak); 
                if(pdf==2) 
		     FgenPoisson.poisson   (storage, area, tmean, rd, frpeak); 
                if(pdf==3) 
		     FgenRandomWalk.ranwlk (storage, area, tmean, rd, skewn, frpeak); 
                if(pdf==4) 
		     FgenGamma.gamvar      (storage, area, tmean, rd, skewn, frpeak); 
                double join = tJoin; 
                int ijoin;
                if(tORfr==1) {
		            ijoin = Math.min(storage.fractionPeak(frJoin),storage.length-1); 
                    join = storage.t[ijoin];
                }
            int itail  = Math.min(storage.lastNonZero(),storage.length)-1;
            double lagendtime= storage.t[itail];
            join= Math.min(join,lagendtime);
            double F   = storage.valueAt(join);            
            double Fm1 = storage.valueAt(join-dt);
            double Fp1 = storage.valueAt(join+dt);
            double S   = (Fp1-Fm1)/(2.0*dt);
            if(expORpow==0) {
                double nexp = nExp;
                if(nexp<1) nexp=1;
                if(nexp>4) nexp=4;
                nexp = Math.floor(nexp);
                if(nexp<4) {w4 = 0; k4 = 0;}
                if(nexp<3) {w3 = 0; k3 = 0;}
                if(nexp<2) {w2 = 0; k2 = 0;}
                // Note that the amplitudes and decay factors are all
                // scaled. The ratios w1/w2, k1/k2, etc are preserved.
                double aw = (w1+w2+w3+w4);
                double a=0;
                if(aw>0) a=F/aw;
                double bwk = (w1*k1+w2*k2+w3*k3+w4*k4);
                double b=0;
                if(bwk>0) b=(-S/F)*aw/bwk;
                for (int i=0; i<storage.length; i++) {
                    if(storage.t[i]>=join) {
                        double now = b*(storage.t[i]-join);
                        storage.ft[i]=a*(w1*Math.exp(-k1*now) +
                            w2*Math.exp(-k2*now) +
                            w3*Math.exp(-k3*now) +
                            w4*Math.exp(-k4*now) ); 
                    }
                }
            }
            if(expORpow==1) {
                double npow = nPow;
                if(npow<1) npow = 1;
                if(npow>4) npow = 4;
                npow = Math.floor(npow);
                double ts1=1; double ts2=1; double ts3=1; double ts4=1;
                if(beta1 <0 ) beta1 = 0;
                if(beta2 <0 ) beta2 = 0;
                if(beta3 <0 ) beta3 = 0;
                if(beta4 <0 ) beta4 = 0;
                if(beta1>0) ts1 = -beta1*F/S;
                if(beta2>0) ts2 = -beta2*F/S;
                if(beta3>0) ts3 = -beta3*F/S;
                if(beta4>0) ts4 = -beta4*F/S;
                if(npow<4) wpow4 = 0;
                if(npow<3) wpow3 = 0;
                if(npow<2) wpow2 = 0;
                double ad = wpow1*Math.pow(ts1,-beta1)+wpow2*Math.pow(ts2,-beta2)
                    +wpow3*Math.pow(ts3,-beta3)+wpow4*Math.pow(ts4,-beta4)  ; 
                double a = 0;
                if(ad>0) a = F/ad;
                for (int i=0; i<storage.length; i++) {
                    if(storage.t[i]>=join) {
                        storage.ft[i]=a*(wpow1*Math.pow(storage.t[i]-join+ts1,-beta1)
                            +wpow2*Math.pow(storage.t[i]-join+ts2,-beta2)
                            +wpow3*Math.pow(storage.t[i]-join+ts3,-beta3)
                            +wpow4*Math.pow(storage.t[i]-join+ts4,-beta4));
                    }
                }
            }
            storage.truncTail(frpeak);
            storage.normalize(area);
            return;
        }
}
