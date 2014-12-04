/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

/* SimAnneal is a Simulated Annealing optimization algorithm
   It is parallelized in the number of parameters being optimized. 
   Reference: S. Kirkpatrick and C. D. Gelatt and M. P. Vecchi, 
   Optimization by Simulated Annealing, 
   Science, Vol 220, Number 4598, pages 671-680, 1983. */

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import java.lang.Math;
import Jama.*;

public class SimAnneal extends Optimizer {
     	
        // static class initialization 
        public static OptimAlg.Info algInfo() {
            OptimAlg.Info algInfo = new OptimAlg.Info();
            algInfo.name = "simanneal";
            algInfo.boundsNeeded = true;
            algInfo.sensMatNeeded = false;
            algInfo.parsNeeded = new String[] { "initTemp" };
            algInfo.optimClassName = SimAnneal.class.getName();
            return algInfo;
        }

        // constructor
        public SimAnneal() { }

        // run optimizer
        public void optimize(RTContext ctxt, OptimResults res, 
        OptimCallbacks cbs) throws Xcept {
            OptimArgs args = res.args;
            int nx = args.nx();        // # of pars to optimize
            double[] x = args.xstart;  // starting values
            double[] xmin = args.xmin; // parameter minimum value
            double[] xmax = args.xmax; // parameter maximum value
            if (Util.hasNaNs(xmin) ||
                Util.hasNaNs(xmax)) throw new Xcept(this,
                "xmin & xmax required");
            double   ssetl = args.errTol;    // mininum error value tolerance
            int      maxfn = args.maxCalls;  // maximum # of function calls
            double[] steps = new double[nx]; // 
            double [][] xpars= new double[nx][nx];
            double err;
            double[] errs = new double[nx];
            int k = 1;
            double scale0 = 1.0/9.1;
            double scale;
            double T = args.initTemp;
	    if (Double.isNaN(T) || T <= 0) throw new Xcept(this,
	    	"Illegal initTemp=" + T);
            double naccept=1;
            double percentAccept=1.00;

            err = cbs.calcError(ctxt,x,res);

            while( (k<=maxfn) && (err>ssetl) ) {
                scale=scale0*(1.0+9.0*(naccept/k -0.1));
                steps=nextStep(ctxt,x,xmin,xmax,scale);
                for (int jx=0; jx<nx; jx++) {
                    for (int ix=0; ix<nx; ix++) { 
                        xpars[jx][ix]=x[ix];
                        if (ix==jx) {xpars[jx][ix] +=steps[ix];}
                   }
                }
                cbs.calcErrors(ctxt, xpars, errs, res) ;
                k+=nx;
                for (int jx=0; jx<nx; jx++) {
                    if (errs[jx]<err) { 
                        naccept++;
                        err=errs[jx];
                        for (int ix=0; ix<nx; ix++) {
                            x[ix] = xpars[jx][ix];    
                        }
                        if(err<ssetl) { sendMsg(res,1); return;}
                    }
                    else {
                      if(err<errs[jx]) {
                          double small = Math.min(0.0005,Math.abs(err+errs[jx]));
                          if( ctxt.random()<Math.exp( (err-errs[jx])/(0.5*(err+errs[jx])/T))) {
                           T=T*0.95;
                           naccept++;
                           err=errs[jx];
                           for (int ix=0; ix<nx; ix++) {
                                x[ix] = xpars[jx][ix];
                           }
                           if(err<ssetl) { sendMsg(res,1); return;}
                   }
                        }
                    }
                }
            } // end while

            if(err<ssetl) { sendMsg(res,1); return;}
            if(k>=maxfn)   { sendMsg(res,4); return;}
            sendMsg(res,0);
            return;
        }

        // error messages
        public void sendMsg(OptimResults res, int istat) {
            switch(istat) {
                case 1:
                    term(res, "mean sqr error");
                    break; 
                 case 4:       
                    term(res,"# calls");
                    break;
                 default:
                    termError(res,"Unspecified error in SimAnneal.");
                    break;
                } 
            return;
        }

        // Generate random steps for parameters and scale them
        public double[] nextStep (RTContext ctxt, double[] x, double[] xmin, double[] xmax, 
            double scale) {
            int nx = x.length;
            double[] step = new double[nx];
            for (int i=0; i<nx; i++) {
            double r = ctxt.random(); 
            double r1 = ctxt.random();
            double r2 = ctxt.random();
                if(r>0.5) 
                    {step[i]= (xmax[i]-x[i])*r1;
               }
                else
                    {step[i]= (xmin[i]-x[i])*r2; 
               }
               step[i]*=scale;
            }
            return step;
        }

        // update OptimResults term info OK
        private void term(OptimResults res, String crit) {
            res.status = OptimResults.NORMAL;
            res.termMsg = "Met " + crit + " stopping criterion";
        }

        // update OptimResults termEror info
        private void termError(OptimResults res, String crit) {
            res.status = OptimResults.ERROR;
            res.termMsg = crit ;
        }

    	// query
    	public String diagInfo() { return "SimAnneal Optimizer"; }
    	public boolean allowMP() { return true; }
}
