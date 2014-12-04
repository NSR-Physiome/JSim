/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

//AUTHOR: Andy Matuszkiewcz

// Defines class MacCormack for solution of convection-diffusion-reaction pde
// by MacCormack explicit method

package JSim.nml.pde1;

import JSim.util.*;
import JSim.jruntime.RTContext;

public class MacCormack {
	private Callbacks callbacks;
	// constructor
	public MacCormack(Callbacks cb) {
	    callbacks = cb;
	}

	// one solver call
	public void solve(RTContext ctxt, boolean useFCT, double ti, double tf,
	double[] xgrid, double[][] ui, double[][] uf) throws Xcept {
	    
	    validate(ctxt, useFCT, ti, tf, xgrid, ui, uf);
	    copyMatrix(ui, uf);

	    // create arrays of coefficents of pdes and initialize them
	    double[][] coefX = new double[ui.length][ui[0].length];
	    double[][] coefXX = new double[ui.length][ui[0].length];
	    double[][] src = new double[ui.length][ui[0].length];

	    // if variable coeffs are allowed need to call at each time step
	    getCoeffs(ctxt, ti, xgrid, ui, coefX, coefXX, src);
	    
	    // do time stepping loop
	    double[][] us = new double[ui.length][ui[0].length];
	    double tS = calcTimeStep(useFCT, xgrid, ui, coefX, coefXX);
	    double tCurr = ti;
	    int maxCt = 10000;
	    int ct = 0;
	    //double prec = 1.0e-7;
	    while (tCurr < tf && ct++ < maxCt) {
	        if (tf - tCurr < tS) {
	            tS = tf - tCurr;
	            tCurr = tf;
	            oneTimeStep(ctxt, useFCT, tCurr, tS, xgrid, uf, us,
	            coefX, coefXX, src);
	            break;
	        }
	        else {
	            tCurr += tS;
	        }
	        oneTimeStep(ctxt, useFCT, tCurr, tS, xgrid, uf, us,
	        coefX, coefXX, src);
	    }
	    if (ct >= maxCt)
	        throw new Xcept("Too many internal time steps in MacCormack solver");
	}
	
	// Calculates time step based on stability requirements
	private double calcTimeStep(boolean useFCT, double[] xgrid,
	double[][] ui, double[][] coefX, double[][] coefXX)  throws Xcept {
	    double tS = 1.0e100;
	    double dum = tS;
	    double dx = 1.0;
	    for (int i = 0; i < ui.length; i++)
	        for (int j = 0; j < ui[0].length-1; j++) {
	            dx = xgrid[j+1] - xgrid[j];
	            dum = 1.0/(Math.abs(coefX[i][j]/dx) + Math.abs(2.0*coefXX[i][j]/dx/dx));
	            if (tS > dum)
	                tS = dum;
	        }
	    return useFCT ? tS/2.0 : tS;
	}
	
	// Advances solution by one internal time step
	private void oneTimeStep(RTContext ctxt, boolean useFCT, double tCurr,
	double tS, double[] xgrid, double[][] uf, double[][] us,
	double[][] coefX, double[][] coefXX, double[][] src)  throws Xcept {
	    // need to check if there is enough points
	    double vel = 0.0;
	    double diff = 0.0;
	    double lFlx = 0.0;
	    double rFlx = 0.0;
	    double dx = xgrid[1] - xgrid[0]; // assume grids are equidistant
	    double lam = tS/dx;
	    double[] f1L = new double[uf.length];
	    double[] f2L = new double[uf.length];
	    double[] f3L = new double[uf.length];
	    double[] f1R = new double[uf.length];
	    double[] f2R = new double[uf.length];
	    double[] f3R = new double[uf.length];
	    
	    // predictor step
	    getCoeffs(ctxt, tCurr, xgrid, uf, coefX, coefXX, src);
	    for (int i = 0; i < us.length; i++) {
	        for (int j = 1; j < us[0].length-1; j++) {
		    vel = coefX[i][j];
	            diff = coefXX[i][j];
	            lFlx = vel*uf[i][j] - diff*(uf[i][j] - uf[i][j-1])/dx;
	            rFlx = vel*uf[i][j+1] - diff*(uf[i][j+1] - uf[i][j])/dx;
	            us[i][j] = uf[i][j] - lam*(rFlx - lFlx) +
	            tS*(src[i][j] + src[i][j+1])/2.0;
	        }
	    }

	    // perform Flux Corrected Transport if needed
	    if (useFCT) {
	        fluxCorrectedTransport(true, tS, dx, uf, us,
	        coefX, coefXX, src);
	    }

            // set BC for predictor step
	    // first order approximation for derivative
	    /*getLeftBC(ctxt, tCurr, uf, coefX, coefXX, f1L, f2L, f3L);
	    for (int i = 0; i < us.length; i ++) {
	        us[i][0] = (f1L[i] != 0.0 || f2L[i] != 0.0) ?
	        (f3L[i] - f2L[i]/dx*us[i][1])/(f1L[i] - f2L[i]/dx) : us[i][1];
	    }
	    getRightBC(ctxt, tCurr, uf, coefX, coefXX, f1R, f2R, f3R);
	    int lj = us[0].length-1;
	    for (int i = 0; i < us.length; i++) {
	        us[i][lj] = (f1R[i] != 0.0 || f2R[i] != 0.0) ?
	        (f3R[i] + f2R[i]/dx*us[i][lj-1])/(f1R[i] + f2R[i]/dx) :
	        us[i][lj-1];
	    }*/

	    // set BC for predictor step
	    // second order approximation for derivative
	    getLeftBC(ctxt, tCurr, uf, coefX, coefXX, f1L, f2L, f3L);
	    for (int i = 0; i < us.length; i ++) {
	        us[i][0] = (f1L[i] != 0.0 || f2L[i] != 0.0) ?
	        -(f2L[i]*(4*us[i][1]-us[i][2]) - 2*f3L[i]*dx)/
	        (2*f1L[i]*dx - 3*f2L[i]) :
	        us[i][1];
	    }
	    getRightBC(ctxt, tCurr, uf, coefX, coefXX, f1R, f2R, f3R);
	    int lj = us[0].length-1;
	    for (int i = 0; i < us.length; i++) {
	        us[i][lj] = (f1R[i] != 0.0 || f2R[i] != 0.0) ?
	        (f2R[i]*(4*us[i][lj-1]-us[i][lj-2]) + 2*f3R[i]*dx)/
	        (2*f1R[i]*dx + 3*f2R[i]) :
	        us[i][lj-1];
	    }

	    // corrector step
	    getCoeffs(ctxt, tCurr, xgrid, us, coefX, coefXX, src);
	    for (int i = 0; i < uf.length; i++) {
	        lFlx = vel*us[i][0] - diff*(us[i][1] - us[i][0])/dx;
	        for (int j = 1; j < uf[0].length-1; j++) {
	            vel = coefX[i][j];
	            diff = coefXX[i][j];
	            lFlx = vel*us[i][j-1] - diff*(us[i][j] - us[i][j-1])/dx;
	            rFlx = vel*us[i][j] - diff*(us[i][j+1] - us[i][j])/dx;
	            uf[i][j] += us[i][j] - lam*(rFlx - lFlx) +
	            tS*(src[i][j-1] + src[i][j])/2.0;
	            uf[i][j] /= 2.0;
	        }
	    }

	    // perform Flux Corrected Transport if needed
	    if (useFCT) {
	        fluxCorrectedTransport(false, tS/2.0, dx, us, uf,
	        coefX, coefXX, src);
	    }

	    // set BC for corector step
	    // first order approximation for derivative
	    /*for (int i = 0; i < uf.length; i ++) {
	        uf[i][0] = (f1L[i] != 0.0 || f2L[i] != 0.0) ?
	        (f3L[i] - f2L[i]/dx*uf[i][1])/(f1L[i] - f2L[i]/dx) : uf[i][1];
	    }
	    lj = uf[0].length-1;
	    for (int i = 0; i < uf.length; i++) {
	        uf[i][lj] = (f1R[i] != 0.0 || f2R[i] != 0.0) ?
	        (f3R[i] + f2R[i]/dx*uf[i][lj-1])/(f1R[i] + f2R[i]/dx) :
	        uf[i][lj-1];
	    }*/

	    // set BC for corector step
	    // second order approximation for derivative
	    for (int i = 0; i < uf.length; i ++) {
	        uf[i][0] = (f1L[i] != 0.0 || f2L[i] != 0.0) ?
	        -(f2L[i]*(4*uf[i][1]-uf[i][2]) - 2*f3L[i]*dx)/
	        (2*f1L[i]*dx - 3*f2L[i]) :
	        uf[i][1];
	    }
	    lj = uf[0].length-1;
	    for (int i = 0; i < uf.length; i++) {
	        uf[i][lj] = (f1R[i] != 0.0 || f2R[i] != 0.0) ?
	        (f2R[i]*(4*uf[i][lj-1]-uf[i][lj-2]) + 2*f3R[i]*dx)/
	        (2*f1R[i]*dx + 3*f2R[i]) :
	        uf[i][lj-1];
	    }
	}

	// Perform a Flux Correction Procedure to avoid spurious oscilations
	public void fluxCorrectedTransport(boolean isPred, double tS, double dx,
	double[][] org, double[][] trg, double[][] coefX, double[][] coefXX,
	double[][] src) throws Xcept {
	    double lam = tS/dx;
	    double eps = 0.0;
	    double[][] nu = new double[org.length][org[0].length];
	    double[][] mu = new double[org.length][org[0].length];
	    for (int i = 0; i < nu.length; i++)
	        for (int j = 0; j < nu[0].length; j++) {
	            eps = Math.abs(coefX[i][j]*lam); // need average ?
	            nu[i][j] = 1.0/6.0 + eps*eps/3.0 - coefXX[i][j]*lam/dx;
	            if (nu[i][j] < 0.0)
	                nu[i][j] = 0.0;
	            mu[i][j] =  1.0/6.0 - eps*eps/6.0 - coefXX[i][j]*lam/dx;
	            if (mu[i][j] < 0.0)
	                mu[i][j] = 0.0;	        
	        }
	    // use antidiffusive fluxes without source terms and num. diff.
	    double[][] trgnf = new double[trg.length][trg[0].length];
	    int pm = (isPred) ? 1 : -1;
	    for (int i = 0; i < trgnf.length; i++) {
	        trgnf[i][0] = trg[i][0] - tS*src[i][0];
	        for (int j = 1; j < trgnf[0].length-1; j++) {
	            trgnf[i][j] = trg[i][j] -
	            tS*(src[i][j] + src[i][j+pm])/2.0;
	        }
	        int lst = trgnf[i].length - 1;
	        trgnf[i][lst] = trg[i][lst] - tS*src[i][lst];
	    }
	    // diffuse previously calculated interior values
	    for (int i = 0; i < trg.length; i++) {
	        int nvar = trg[i].length;
	        for (int j = 1; j < nvar - 1; j++) {
	            trg[i][j] += nu[i][j]*(org[i][j+1] - 2.0*org[i][j] + org[i][j-1]);
	        }
	    }
	    // calculate antidiffusive fluxes and store them in flx
	    double[][] flx = new double[trg.length][trg[0].length];
	    for (int i = 0; i < flx.length; i++) {
	        int nvar = flx[i].length;
	        for (int j = 0; j < nvar - 1; j++) {
	            flx[i][j] = mu[i][j]*(trgnf[i][j+1] - trgnf[i][j]);
	        }
	    }
	    // calculate corrected fluxes and overwirte them in flx
	    for (int i = 0; i < flx.length; i++) {
	        int nvar = trgnf[i].length;
	        // first element calculated by different formula
	        double s = sign(trgnf[i][1] - trgnf[i][0]);
	        flx[i][0] = s*Math.max(0.0,
	        Math.min(s*(trgnf[i][2]-trgnf[i][1]), Math.abs(flx[i][0])));
	        for (int j = 1; j < nvar-2; j++) {
	            s = sign(trgnf[i][j+1] - trgnf[i][j]);
	            flx[i][j] = s*Math.max(0.0,
	            min(s*(trgnf[i][j+2]-trgnf[i][j+1]), Math.abs(flx[i][j]),
	            s*(trgnf[i][j]-trgnf[i][j-1])));
	        }
	        s = sign(trgnf[i][nvar-1] - trgnf[i][nvar-2]);
	        flx[i][nvar-2] = s*Math.max(0.0,
	        Math.min(Math.abs(flx[i][nvar-2]),
	        s*(trgnf[i][nvar-2]-trgnf[i][nvar-3])));
	    }
	    // calculate corrected variables and store them in trg
	    for (int i = 0; i < flx.length; i++) {
	        int nvar = trg[i].length;
	        for (int j = 1; j < nvar - 1; j++) {
	            trg[i][j] -= flx[i][j] - flx[i][j-1];
	        }
	    } // end for i
	}

	// gets pde's coefficients by a callback
	public void getCoeffs(RTContext ctxt, double t, double[] xgrid,
	double[][] uC, double[][] coefX, double[][] coefXX,
	double[][] src) throws Xcept {
	    double[] cX = new double[coefX.length];
	    double[] cXX = new double[coefXX.length];
	    double[] s = new double[src.length];
	    double[] u = new double[uC.length];
	    for (int j = 0; j < coefX[0].length; j++) {
	        for (int i = 0; i < u.length; i++) {
	            u[i] = uC[i][j];
	        }
	        callbacks.MacCormack_State(ctxt, t, xgrid[j], u,
	        cX, cXX, s);
	        for (int i = 0; i < cX.length; i++) {
	            coefX[i][j] = cX[i];
	            coefXX[i][j] = cXX[i];
	            src[i][j] = s[i];
	        }
	    }
	}

	// gets left BC by a callback
	public void getLeftBC(RTContext ctxt, double t, double[][] uC,
	double[][] coefX, double[][] coefXX, double[] f1,
	double[] f2, double[] f3) throws Xcept {
	    double[] cX = new double[coefX.length];
	    double[] cXX = new double[coefXX.length];
	    double[] u = new double[uC.length];
	    for (int i = 0; i < u.length; i++) {
	        cX[i] = coefX[i][0];
	        cXX[i] = coefXX[i][0];
	    }
	    callbacks.common_LHB(ctxt, t, u, f1, f2, f3);
	    for (int i = 0; i < u.length; i++)
	        uC[i][0] = u[i];
	}

	// gets right BC by a callback
	public void getRightBC(RTContext ctxt, double t, double[][] uC,
	double[][] coefX, double[][] coefXX,
	double[] f1, double[] f2, double[] f3) throws Xcept {
	    double[] cX = new double[coefX.length];
	    double[] cXX = new double[coefXX.length];
	    double[] u = new double[uC.length];
	    for (int i = 0; i < u.length; i++) {
	        cX[i] = coefX[i][coefX[0].length-1];
	        cXX[i] = coefXX[i][coefX[0].length-1];
	    }
	    callbacks.common_RHB(ctxt, t, u, f1, f2, f3);
	    for (int i = 0; i < u.length; i++)
	        uC[i][uC[i].length-1] = u[i];
	}

	// copy src to trg
	public void copyMatrix(double[][] src, double[][] trg) {
	    // may need length checking
	    for (int i = 0; i < src.length; i++)
	        for (int j = 0; j < src[0].length; j++)
	            trg[i][j] = src[i][j];
	}

	// auxilliary math function
	public double sign(double x) {
	    return (x < 0.0) ? -1.0 : 1.0;
	}

	public double min(double a, double b, double c) {
	    return Math.min(a, Math.min(b,c));
	}

	public void validate(RTContext ctxt, boolean useFCT, double ti,
	double tf, double[] xgrid, double[][] ui, double[][] uf) throws Xcept {
	    String macStr = "in MacCormack solver";

	    if (tf < ti)
	        throw new Xcept("Final time less then initial time in MacCormack solver");

	   for (int i = 0; i < ui.length; i++) {
	       if (Double.isNaN(ui[i][0]))
	           throw new Xcept("NaN detected at LHB " + macStr);
	       if (Double.isNaN(ui[i][ui[i].length-1]))
	           throw new Xcept("NaN detected at RHB " + macStr);
	    }
	}

	// Callbacks sub-interface
	public static interface Callbacks {
	    // gets user defined coefficients of partial differential equations
	    public void MacCormack_State(RTContext ctxt, double t,
	    double x, double[] uc, double[] cX, double[] cXX, double[] s)
	    throws Xcept;

	    // gets user defined left boundary conditions
	    public void common_LHB(RTContext ctxt, double t,
	    double[] uc, double[] f1, double[] f2, double[] f3) throws Xcept;

	    // gets user defined right boundary conditions
	    public void common_RHB(RTContext ctxt, double t,
	    double[] uc, double[] f1, double[] f2, double[] f3) throws Xcept;
	}
}
