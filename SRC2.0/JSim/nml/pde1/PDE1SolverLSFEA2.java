/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// lagrangian sliding fluid element PDE solver - version 2

package JSim.nml.pde1;

import java.lang.Math;
import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*; 
import JSim.nml.*;
import JSim.nml.ode1.*;

public class PDE1SolverLSFEA2 extends PDE1Slave 
implements ODE1Callbacks {
  	public final static String solverName = "LSFEA2";
	private ODE1Solver odeSolver; // for t-step

	// state saved in one pass through t
	//   this requires isReentrant() = false
 	private boolean firstTime;
  	private boolean canContinue;
	private double[] xitg;
  	private double timein;
  	private double[] ont0;
  	private double[][] wt;
  	private int[] nwt;
  	private boolean[] ondfr;
  	private double[] vel;
	private double odeXValue; // curr X value for odeSolver
	private double[] zeroes; // u=0 array passed to BC callbacks
	private double[] f1, f2, f3; // BC callback arrays
	private double[] g1, g2, g3; // BC callback arrays

	// constructor
  	public PDE1SolverLSFEA2(PDE1Solver s) throws Xcept {
	    super(s, solverName);
	    odeSolver = new ODE1Solver(this, -1, 
	        solver.namedVals(), npde());
	    firstTime = true;
	    canContinue = true;
	    odeXValue = Double.NaN;
	    zeroes = new double[npde()];
	    f1 = new double[npde()];
	    f2 = new double[npde()];
	    f3 = new double[npde()];
	    g1 = new double[npde()];
	    g2 = new double[npde()];
	    g3 = new double[npde()];
	}

	// is solver reentrant?
	public boolean isReentrant() { return false; }

	// ODE1Callbacks
	public void evaluate(RTContext ctxt, double t, 
	double[] u, double[] udot) throws Xcept {
	    callbacks().LSFEA_tstep(ctxt, t, odeXValue, u, udot);
	}

	// main solve method
	public void solve(RTContext ctxt,
	double t0, double tfinal, double[] xgrid,
	double[][] u0, double[][] uf,
	double[][] ut0, double[][] utf,
	double[][] ux0, double[][] uxf,
	double[][] uxt0, double[][] uxtf,
	double[] v0, double[] vf,
	double[] vt0, double[] vtf) throws Xcept {
	    for (int i = 0; i < u0.length; i++)
	      for (int j = 0; j < u0[0].length; j++)
	        uf[i][j] = u0[i][j];

	    //B.C.
	    callbacks().common_LHB(ctxt, t0, zeroes, f1, f2, f3);
	    callbacks().common_RHB(ctxt, t0, zeroes, g1, g2, g3);
	    for (int i = 0; i < u0.length; i++) {
	        if (f1[i] != 0) 
	  	    u0[i][0] = uf[i][0] = f3[i]/f1[i];
	        if (g1[i] != 0)
	            u0[i][u0[0].length-1] = 
		    uf[i][u0[0].length-1] = g3[i]/g1[i];
		if (f1[i] != 0 && g1[i] != 0)
		    fail(i, "At least one of f1 and g1 must be zero");
		if (f2[i] == 0 && g2[i] == 0)
		    fail(i, "At least one of f2 and g2 must be non-zero");
	    }

	    double[][] uxxf = new double[u0.length][u0[0].length];

	    oneTStep(ctxt, t0, tfinal, xgrid, uf, utf, uxf, uxxf);
	}

	// LSFEA failure: promote to another solver
	private void fail(String msg) throws Xcept {
	    throw new Xcept("LSFEA PDE solver: " + msg + 
	    	" (use MacCormack or Toms731 instead)");
	}
	private void fail(int inx, String msg) throws Xcept {
	    throw new Xcept("LSFEA PDE solver - variable " + 
	        getVarName(inx) + ": " + msg + 
	    	" (use MacCormack or Toms731 instead)");
	}

	// one time-step ?
	public void oneTStep(RTContext ctxt,
	double t, double tend, double[] xgrid, double[][] u,
        double[][] ut, double[][] ux, double[][] uxx) 
	throws Xcept {

	    if (!canContinue) return;

	    double[] coefX  = new double[u.length];
	    double[] coefXX = new double[u.length];
	    double[] uu = new double[u.length * u[0].length];

	    callbacks().LSFEA_xstep(ctxt, t, xgrid[0], uu, coefX, coefXX);
	    int neqn = coefX.length;
	    int ngrid = xgrid.length;
	    
	    // LSFEA sanity checks
	    for (int i=0; i<neqn; i++) {
	        if (coefXX[i] < 0) throw new Xcept(
		    "LSFEA solver failure: negative diffusion term");
		if (coefX[i] == 0) {
		    if (f1[i] != 0 || f3[i] != 0 || g1[i] != 0 || g3[i] != 0)
		    	fail(i, "Zero advection requires zero f1, f3, g1 and g3");
		} else if (coefX[i] > 0) {
		    if (f1[i] == 0) 
		        fail(i, "Positive advection requires non-zero f1");
		} else if (coefX[i] < 0) {
		    if (g1[i] == 0)	
		        fail(i, "Negative advection requires non-zero g1");
		}
	    }
	    if (ngrid % 2 == 0) {
	        canContinue = false; // still needed???
	        fail("Number of grid points must be an odd");
	    }

	    int nseg = (ngrid-1)/2;
	    double xlen = xgrid[xgrid.length-1] - xgrid[0];
	    double dt = tend - t;

	    // calculate velocities and numeric time step
	    double tStepSize = dt;
	    vel = new double[neqn];
	    for (int i = 0; i < neqn; i++) {
	      vel[i] =  coefX[i] / xlen;
	      if (Math.abs(vel[i]) < 0.000000001) vel[i] = 0;
	    }
	    double minVel = 0;
	    for (int i = 0; i < neqn; i++) {
	      if (vel[i] != 0 && Math.abs(vel[i]) < minVel || minVel == 0 )
	        minVel = Math.abs(vel[i]);
	    }
	    if (minVel != 0)
	      tStepSize = 1 / (nseg*minVel);
	    else {
	      canContinue = false;
	      throw new Xcept(
	        "Solver failure: convection terms cannot be all zero. Try other solvers.");
	    }

	    // first call only
	    if (firstTime) {
	      firstTime = false;
	
	      timein = t - dt;
	
	      xitg = new double[neqn];
	      ont0 = new double[neqn];
	      nwt  = new int[neqn];
	      ondfr = new boolean[neqn];
	      wt = new double[neqn][2*nseg];
	      for (int i = 0; i < neqn; i++) {
	        xitg[i] = 0;
	        ont0[i] = 0;
	
	        ondfr[i] = false;
	        if (coefXX[i] > 0 && nseg>=3) {
	          nwt[i] = ddifcf(nseg, xlen, tStepSize, coefXX[i], wt[i]);
	
	          if (nseg >= 3 && xlen > 0)
	            ondfr[i] = true;
	        }
	      }
	    }

	    double[][] useg = new double[neqn][nseg+2];
	    gridToSeg(neqn, useg, u);

	    // calculate velocity ratios
	    double[] velRatio = new double[neqn];
	    double[] uin = new double[neqn];
	    double[] oitg = new double[neqn];
	    double[] uout = new double[neqn];
	    for (int i = 0; i < neqn; i++) {
	      if (minVel > 0)
	        velRatio[i] = vel[i] / minVel;
	      else
	        velRatio[i] = 0;
	
	      if (vel[i] > 0)
	        uin[i] = u[i][0];
	      else if (vel[i] < 0)
	        uin[i] = u[i][ngrid-1];
	      else
	        uin[i] = 0;
	
	      if (Double.isNaN(uin[i])) uin[i] = 0;
	
	      xitg[i] += uin[i] * dt;
	
	      oitg[i] = 0;
	      uout[i] = 0;
	    }

	    double[] uode = new double[neqn];
	    double[] uodedot = new double[neqn];
	    double t0, xtmp, uavg, oamt=0;
	    double currx;
	    int nstep = (int) ((t-timein) / tStepSize);
	    if (nstep > 0) {
	      for (int k = 0; k < nstep; k++) {
	
	        // convection using sliding algorithm
	        t0 = timein;
	        timein += tStepSize;
	        for (int i = 0; i < neqn; i++) {
	          if (velRatio[i] != 0) {
	            xtmp = (t-timein) * uin[i];
	            uavg = (xitg[i] - xtmp) / tStepSize;
	            xitg[i] = xtmp;
	            if (vel[i] > 0)
	              useg[i][0] = uavg;
	            else if (vel[i] < 0)
	              useg[i][nseg+1] = uavg;
	
	            oamt = slide(useg[i], velRatio[i], tStepSize, true);
	            oitg[i] += oamt;
	          }
	        }

	        // exchange and reaction by solving a set of ODE's for every segment
	        for (int j = 1; j <= nseg; j++) {
	          int jinx = 2*j - 1;
	          odeAryAt(uode, useg, j);	
		  odeXValue = callbacks().LSFEA_setX(ctxt, jinx);
		  // below: uode unsafe???
	 	  odeSolver.solve(ctxt, t0, t0+tStepSize, uode, uode);
	          pdeAryAt(useg, uode, j);
	        }

	        // axial diffusion
	        for (int i = 0; i < neqn; i++)
	          if (ondfr[i]) dwmwav(nseg, useg[i], nwt[i], wt[i]);
	      }
	    }

	    // fractional outflow
	    double textra = t - timein;
	    for (int i = 0; i < neqn; i++) {
	      if (velRatio[i] != 0) {
	        oamt = slide(useg[i], velRatio[i], textra, false);
	        oitg[i] += (oamt - ont0[i]);
	        ont0[i] = oamt;
	      }

	      uout[i] = oitg[i] / dt;
	      if (vel[i] > 0) {
	        useg[i][nseg+1] = uout[i];
	        u[i][ngrid-1] = uout[i];
	      } else if (vel[i] < 0) {
	        useg[i][0] = uout[i];
	        u[i][0] = uout[i];
	      }
	    }

	    segToGrid(neqn, u, useg);
	}

	// slide method
	public double slide(double[] useg, double vRatio, 
	double dt, boolean flg) throws Xcept {

	    double oamt = 0;


	    if (Math.abs(vRatio) < 0.000001) return oamt;

	    int nseg = useg.length-2;
	    int ish = (int) Math.abs(vRatio);
            int ngridcheck=2*ish+1;
	    if(nseg<ish) throw new Xcept
	        ("Ngrid for LSFEA solver must be at least "+ngridcheck+" because of velocity ratios"); 
	    double extra = Math.abs(vRatio) - ish;
	    if (vRatio > 0) {
	      for (int i = 1; i <= ish; i++) {
	        oamt += useg[nseg+1-i]; }

	      oamt += extra * useg[nseg-ish];

	    } else {
	      for (int i = 1; i <= ish; i++)
	        oamt += useg[i];

	      oamt += extra * useg[ish+1];

	    }
	    oamt = oamt / Math.abs(vRatio) * dt;

	    if (!flg) return oamt;

	    double scal1 = extra;
	    double scal2 = 1 - extra;
	    if (vRatio > 0) {
	      if (scal1 == 0) {
	        for (int i = nseg; i >= ish+1; i--)
	          useg[i] = useg[i-ish];
	      } else {
	        for (int i = nseg; i >= ish+1; i--)
	          useg[i] = scal1*useg[i-ish-1] + scal2*useg[i-ish];
	      }

	      for (int i = 1; i <= ish; i++)
	        useg[i] = useg[0];

	    } else {
	      if (scal1 == 0) {
	        for (int i = 1; i <= nseg-ish; i++) {
	          useg[i] = useg[i+ish];
	        }
	      } else {
	        for (int i = 1; i <= nseg-ish; i++)
	          useg[i] = scal1*useg[i+ish+1] + scal2*useg[i+ish];
	      }

	      for (int i = nseg-ish+1; i <= nseg; i++)
	        useg[i] = useg[nseg+1];
	    }

	    return oamt;
	}

	// AM 27 Oct 2005: to fix a bug reported by JB
	//     difference between an analytical solution and lsfea
	public void gridToSeg(int neqn, double[][] useg, double[][] u) {
	    int nseg = useg[0].length;
	    int ngrid = u[0].length;

	    int i, j;
	    for (i = 0; i < neqn; i++) {
	        useg[i][0] = u[i][0];
	        useg[i][nseg-1] = u[i][ngrid-1];
	        for (j = 1; j < nseg-1; j++) {
		    int j2 = (vel[i]<0) ? 2*j-2 : 2*j;
	            useg[i][j] =  u[i][j2];
               }
	    }
	}
	public void segToGrid(int neqn, double[][] u, double[][] useg) {
	    int nseg = useg[0].length;
	    int ngrid = u[0].length;

	    int i, j;
	    for (i = 0; i < neqn; i++) {
	        u[i][0] = useg[i][0];
	        u[i][ngrid-1] = useg[i][nseg-1];
	        for (j = 1; j < nseg-1; j++) {
	            if(vel[i]<0)
		        u[i][2*j-2] = useg[i][j];
                    else 
		        u[i][2*j] = useg[i][j];

	            // interpolation 
	            if (j < nseg) {
               // insertion by GMR 04/23/2010
	                if(vel[i]>=0) 
               // deletion by GMR 04/23/2010 
               //   if(vel[i]>0)
			    u[i][2*j-1] = (useg[i][j-1] + useg[i][j])/2;
                        else 
			    u[i][2*j-1] = (useg[i][j] + useg[i][j+1])/2;
                    }
	        }

	        if (vel[i] == 0) {
              u[i][0] = u[i][1];
              u[i][1] = 2*u[i][2]-u[i][3];
	        }
	    }
	}
	// end of modification

	// ?
	public void odeAryAt(
	double[] des, double[][] src, int j) {
	    for (int i = 0; i < des.length; i++)
	        des[i] = src[i][j];
	}

	// ?
	public void pdeAryAt(
	double[][] des, double[] src, int j) {
	    for (int i = 0; i < src.length; i++)
	        des[i][j] = src[i];
	}

	// ?
	public int ddifcf(int nseg, double xlen, 
	double dt, double d, double[] w) {

	    int nw;

	    double dx = xlen / nseg;
	    double dx2 = dx / 2;
	    double diff = d * 2*dt / (dx*dx);

	    if (diff <= 0.25 || nseg <= 5) {
	        nw = 3;
	        w[1] = Math.exp(-diff);
	        w[0] = 0.5 * (1 - w[1]);
	        w[2] = w[0];

	        return nw;
	    }

	    int nhafp1 = (int) Math.min((int) (4 * Math.sqrt(diff)), nseg-1) + 1;
	    nw = 2*nhafp1 - 1;
	    double scale = 0.5 / Math.sqrt(d * dt);
	    double x = 0;
	    double sum = 0;

	    for (int i = nhafp1-1; i >= 0; i--) {
	        w[i] = 0.5 * (erf((x+dx2)*scale) - erf((x-dx2)*scale));
	        w[nw-1-i] = w[i];
	        sum += w[i];
	        x += dx;
	    }

	    sum = 2*sum - w[nhafp1-1];

	    double rsum = 1;
	    if (sum > 0) {
	        rsum = 1/sum;
	        for (int i = 0; i < nw; i++)
	            w[i] *= rsum;
	    }

	    return nw;
	}

	// ?
  	public void dwmwav(int nf, double[] f, 
	int nspan, double[] w) {

	    double[] wk = new double[nf+nspan];

	    int nhalf = nspan / 2;

	    if (nhalf > nf || nspan <= 1) return;

	    for (int i = 0; i < nf; i++) {
	        wk[i+nhalf] = f[i+1];
	        f[i+1] = 0;
	    }

	    int index = nhalf;
	    for (int i = 0; i < nhalf; i++) {
	        index--;
	        wk[index] = wk[i+nhalf];
	        wk[nf+i+nhalf] = wk[nf-i+nhalf-1];
	    }

	    for (int j = 0; j < nf; j++)
	        for (int i = 0; i < nspan; i++)
	            f[j+1] += w[i] * wk[j+i];
	}

	// ?
	public double erf(double x) {
	    double a1 =  0.254829592;
	    double a2 = -0.284496736;
	    double a3 =  1.421413741;
	    double a4 = -1.453152027;
	    double a5 =  1.061405429;
	    double scale = 0.3275911000;
	    double c1 = 1.1283791671;
	    double c2 = 0.6666666666;
	    double c3 = 0.2666666666;

	    double ax, ax2, ax3, z, fval;

	    ax = Math.abs(x);
	    ax2 = ax * ax;

	    if (ax > 0.1) {
	        z = 1 / (1+scale*ax);
	        fval = 1 - z * Math.exp(-ax2)*(a1 + z*(a2 + z*(a3 + z*(a4 + z*a5))));
	    } else {
	        ax3 = ax * ax2;
	        fval = c1 * Math.exp(-ax2) * (ax + c2*ax3 + c3*ax3*ax2);
	    }

	    if (x < 0) fval = -fval;

	    return fval;
  	}
}
