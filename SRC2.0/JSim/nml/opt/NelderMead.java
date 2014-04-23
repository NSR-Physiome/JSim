/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// JSim adaptation of Nelder-Mead implementation by Flanagan
// mods by EB Sep 2006
// source from http://www.ee.ucl.ac.uk/~mflanaga/java/Minimisation.html

/*
*   WRITTEN BY: Dr Michael Thomas Flanagan
*
*   DATE:        April 2003
*   MODIFIED:   29 December 2005, 18 February 2006
*
*   DOCUMENTATION:
*   See Michael Thomas Flanagan's Java library on-line web page:
*   NelderMead.html
*
*   Copyright (c) April 2003
*
*   PERMISSION TO COPY:
*   Permission to use, copy and modify this software and its documentation for
*   NON-COMMERCIAL purposes is granted, without fee, provided that an acknowledgement
*   to the author, Michael Thomas Flanagan at www.ee.ucl.ac.uk/~mflanaga, appears in all copies.
*
*   Dr Michael Thomas Flanagan makes no representations about the suitability
*   or fitness of the software for any or for a particular purpose.
*   Michael Thomas Flanagan shall not be liable for any damages suffered
*   as a result of using, modifying or distributing this software or its derivatives.
*
*/

package JSim.nml.opt;

import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*;
import java.util.*;

// NelderMead class
public class NelderMead extends Optimizer {

	// static class initialization 
	public static OptimAlg.Info algInfo() {
	    OptimAlg.Info algInfo = new OptimAlg.Info();
	    algInfo.name = "neldermead";
	    algInfo.boundsNeeded = false;
	    algInfo.sensMatNeeded = false;
	    algInfo.parsNeeded = new String[] { "xstep" };
	    algInfo.optimClassName = NelderMead.class.getName();
	    return algInfo;
	}

    // constructor
    public NelderMead() {    
    }

    // optimize method
    public void optimize(RTContext ctxt, OptimResults res, 
    OptimCallbacks cbs) throws Xcept {

	// original global vars
        int nParam=0;                   // number of unknown parameters to be estimated
        double[]paramValue = null;      // function parameter values (returned at function minimum)
        String[] paraName = null;       // names of parameters, eg, c[0], c[1], c[2] . . .
        double functValue = 0.0D;       // current value of the function to be minimised
        double minimum = 0.0D;          // value of the function to be minimised at the minimum
        int prec = 4;                   // number of places to which double variables are truncated on output to text files
        int field = 13;                 // field width on output to text files
        boolean convStatus = false;     // Status of minimisation on exiting minimisation method
                                            // = true  -  convergence criterion was met
                                            // = false -  convergence criterion not met - current estimates returned
        int nMax = 3000;                //  Nelder and Mead simplex maximum number of iterations
        int nIter = 0;                  //  Nelder and Mead simplex number of iterations performed
        int konvge = 3;                 //  Nelder and Mead simplex number of restarts allowed
        int kRestart = 0;               //  Nelder and Mead simplex number of restarts taken
        double fTol = 1e-13;             //  Nelder and Mead simplex convergence tolerance
        double rCoeff = 1.0D;           //  Nelder and Mead simplex reflection coefficient
        double eCoeff = 2.0D;           //  Nelder and Mead simplex extension coefficient
        double cCoeff = 0.5D;           //  Nelder and Mead simplex contraction coefficient
        double[] step = null;           //  Nelder and Mead simplex step values
        double dStep = 0.5D;            //  Nelder and Mead simplex default step value
        int minTest = 0;                //  Nelder and Mead minimum test
                                            //      = 0; tests simplex sd < fTol
                                            //  allows options for further tests to be added later
        double simplexSd = 0.0D;        //  simplex standard deviation

	// load JSim arguments
        double[] start = (double[]) res.args.xstart.clone();
	step = (double[]) res.args.xistep.clone();
 	fTol = res.args.errTol;
	nMax = res.args.maxCalls;

	// start original NelderMead() method
        boolean testContract=false; // test whether a simplex contraction has been performed
        int np = start.length;  // number of unknown parameters;
        nParam = np;
        convStatus = true;
        int nnp = np+1; // Number of simplex apices

        if(step.length!=start.length)throw new Xcept(this, "step array length " + step.length + " and initial estimate array length " + start.length + " are of different");

        // check for zero step sizes
        for(int i=0; i<np; i++)if(step[i]==0.0D)throw new Xcept(this, 
	    "step " + i+ " size is zero");

        // set up arrays
        paramValue = new double[np];
        double[]pmin = new double[np];   //Nelder and Mead Pmin

        double[][] pp = new double[nnp][nnp];   //Nelder and Mead P
        double[] yy = new double[nnp];          //Nelder and Mead y
        double[] pbar = new double[nnp];        //Nelder and Mead P with bar superscript
        double[] pstar = new double[nnp];       //Nelder and Mead P*
        double[] p2star = new double[nnp];      //Nelder and Mead P**

        // set class member values
        nIter=0;

        // initial simplex
        double sho=0.0D;
        for (int i=0; i<np; ++i){
            sho=start[i];
            pstar[i]=sho;
            p2star[i]=sho;
            pmin[i]=sho;
        }

        int jcount=konvge;  // count of number of restarts still available

	// 1st nnp fcn's: pp[][] -> yy[]
        for (int i=0; i<np; ++i){
            for (int j=0; j<np; ++j) {
                pp[j][i]=start[i];
 		if (i==j) pp[j][i] += step[j];
	    }
            pp[nnp-1][i]=start[i];
	}
	fcn1(nnp, cbs, ctxt, pp, yy, res);
	
        // loop over allowed iterations
        double  ynewlo=0.0D;    // current value lowest y
        double     ystar = 0.0D;   // Nelder and Mead y*
        double  y2star = 0.0D;  // Nelder and Mead y**
        double  ylo = 0.0D;     // Nelder and Mead y(low)
        double  fMin;   // function value at minimum
        // variables used in calculating the variance of the simplex at a putative minimum
        double     curMin = 00D, sumnm = 0.0D, summnm = 0.0D, zn = 0.0D;
        int ilo=0;  // index of low apex
        int ihi=0;  // index of high apex
        int ln=0;   // counter for a check on low and high apices
        boolean test = true;    // test becomes false on reaching minimum

        while(test){
            // Determine h
            ylo=yy[0];
            ynewlo=ylo;
            ilo=0;
            ihi=0;
            for (int i=1; i<nnp; ++i){
                if (yy[i]<ylo){
                    ylo=yy[i];
                    ilo=i;
                }
                if (yy[i]>ynewlo){
                    ynewlo=yy[i];
                    ihi=i;
                }
            }
            // Calculate pbar
            for (int i=0; i<np; ++i){
                zn=0.0D;
                for (int j=0; j<nnp; ++j){
                    zn += pp[j][i];
                }
                zn -= pp[ihi][i];
                pbar[i] = zn/np;
            }

            // Calculate p=(1+alpha).pbar-alpha.ph {Reflection}
            for (int i=0; i<np; ++i)
	        pstar[i]=(1.0 + rCoeff)*pbar[i]-rCoeff*pp[ihi][i];

            // Calculate y*
            ystar=fcn1(cbs, ctxt, pstar, res);

            ++nIter;

            // check for y*<yi
            if(ystar < ylo){
                // Form p**=(1+gamma).p*-gamma.pbar {Extension}
                for (int i=0; i<np; ++i)
		    p2star[i]=pstar[i]*(1.0D + eCoeff)-eCoeff*pbar[i];
                // Calculate y**
                y2star=fcn1(cbs, ctxt, p2star, res);
                ++nIter;
                if(y2star < ylo){
                    // Replace ph by p**
                    for (int i=0; i<np; ++i)pp[ihi][i] = p2star[i];
                    yy[ihi] = y2star;
                }
                else{
                    //Replace ph by p*
                    for (int i=0; i<np; ++i)pp[ihi][i]=pstar[i];
                    yy[ihi]=ystar;
                }
            }
            else{
                // Check y*>yi, i!=h
                ln=0;
                for (int i=0; i<nnp; ++i)if (i!=ihi && ystar > yy[i]) ++ln;
                if (ln==np ){
                    // y*>= all yi; Check if y*>yh
                    if(ystar<=yy[ihi]){
                        // Replace ph by p*
                        for (int i=0; i<np; ++i)pp[ihi][i]=pstar[i];
                        yy[ihi]=ystar;
                    }
                    // Calculate p** =beta.ph+(1-beta)pbar  {Contraction}
                    for (int i=0; i<np; ++i)p2star[i]=cCoeff*pp[ihi][i] + (1.0 - cCoeff)*pbar[i];
                    // Calculate y**
                    y2star=fcn1(cbs, ctxt, p2star, res);
                    ++nIter;
                    // Check if y**>yh
                    if(y2star>yy[ihi]){
                        //Replace all pi by (pi+pl)/2

                        for (int j=0; j<nnp; ++j){
                            for (int i=0; i<np; ++i){
                                pp[j][i]=0.5*(pp[j][i] + pp[ilo][i]);
                                pmin[i]=pp[j][i];
                            }
                        }
			fcn1(nnp, cbs, ctxt, pp, yy, res);
                        nIter += nnp;
                    }
                    else{
                        // Replace ph by p**
                        for (int i=0; i<np; ++i)pp[ihi][i] = p2star[i];
                        yy[ihi] = y2star;
                    }
                }
                else{
                    // replace ph by p*
                    for (int i=0; i<np; ++i)pp[ihi][i]=pstar[i];
                    yy[ihi]=ystar;
                }
            }

            // test for convergence
            // calculte sd of simplex and minimum point
            sumnm=0.0;
            ynewlo=yy[0];
            ilo=0;
            for (int i=0; i<nnp; ++i){
                sumnm += yy[i];
                if(ynewlo>yy[i]){
                    ynewlo=yy[i];
                    ilo=i;
                }
            }
            sumnm /= (double)(nnp);
            summnm=0.0;
            for (int i=0; i<nnp; ++i){
                zn=yy[i]-sumnm;
                summnm += zn*zn;
            }
            curMin=Math.sqrt(summnm/np);

	    // modified sum sqr error test for JSim
	    for (int i=0; i<nnp; i++) {
	        if (yy[i] >= fTol) continue;
	        res.status = OptimResults.NORMAL;
		res.termMsg = "Met mean sqr error stopping criterion";
 		test=false;
            }
            // test simplex sd: OLD VERSION 
	    // different interp of fTol conflicts w/ JSim expectation
//          if (minTest==0 && curMin<fTol) {
//	        res.status = OptimResults.NORMAL;
//		res.termMsg = "Met  stopping criterion";
// 		test=false;
//          }

            minimum=ynewlo;
            if(!test){
                // store parameter values
                for (int i=0; i<np; ++i)pmin[i]=pp[ilo][i];
                yy[nnp-1]=ynewlo;
                // store simplex sd
                simplexSd = curMin;
                // test for restart
                --jcount;
                if(jcount>0){
                    test=true;
                    for (int j=0; j<np; ++j) {
                        pmin[j]=pmin[j]+step[j];
                        for (int i=0; i<np; ++i)pp[j][i]=pmin[i];
                        pmin[j]=pmin[j]-step[j];
                    }
		    fcn1(np, cbs, ctxt, pp, yy, res);
                }
            }

            if(test && nIter>nMax){
                convStatus = false;
                // store current estimates
                for (int i=0; i<np; ++i)pmin[i]=pp[ilo][i];
                yy[nnp-1]=ynewlo;
		res.status = OptimResults.NORMAL;
		res.termMsg = "Met # calls stopping criterion";
                test=false;
            }
        }

        for (int i=0; i<np; ++i){
            pmin[i] = pp[ihi][i];
            paramValue[i] = pmin[i];
        }
        minimum=ynewlo;
        kRestart=konvge-jcount;

    }

    // call single fcn with 1-smaller x array (np/nnp allocations)
    public double fcn1(OptimCallbacks cbs, RTContext ctxt, double[] x,
    OptimResults res) throws Xcept {
        double[] x1 = new double[x.length-1];
	for (int i=0; i<x1.length; i++) x1[i] = x[i];
	double fx = cbs.calcError(ctxt, x1, res);
	return fx;
    }

    // call mult fcn with 1-smaller x array (np/nnp allocations)
    public int fcn1(int nc, OptimCallbacks cbs, RTContext ctxt, double[][] x, double[] fx,
    OptimResults res) throws Xcept {
	int np = x[0].length - 1;
	double[][] x1 = new double[nc][np];
	for (int i=0; i<nc; i++)
	    for (int j=0; j<np; j++)
	        x1[i][j] = x[i][j];
	double[] fx1 = new double[nc];	
	int ret = cbs.calcErrors(ctxt, x1, fx1, res);
	for (int i=0; i<nc; i++)
	    fx[i] = fx1[i];
	return ret;
    }

    //// EB additions
    public double square(double d) { return d*d; }
    public double truncate(double d, double prec) { return d; }
    public boolean allowMP() { return true; }
    public String diagInfo() { return "Nelder-Mead optimizer"; }

}
