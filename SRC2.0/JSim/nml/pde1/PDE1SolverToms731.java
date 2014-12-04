/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

package JSim.nml.pde1;

/**
 * Title:
 * Description:
 * Copyright:    Copyright (c) 2001
 * Company:
 * @author
 * @version 1.0
 */

import java.lang.Math;
import JSim.nml.*;
import JSim.nml.ode1.*;
import JSim.util.*;
import JSim.jruntime.RTContext;
import JSim.data.*; 

public class PDE1SolverToms731 extends PDE1Slave {
  	public final static String solverName = "Toms731 (moving-grid)";
  	private int neqn;
	private int ngrid;

	// load native lib	
	static { System.loadLibrary("pdesolver"); }

	// constructor
	public PDE1SolverToms731(PDE1Solver s) throws Xcept {
	    super(s, solverName);
	}

	// solve method
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
	    double[] zeros = new double[u0.length];
	    double[] beta = new double[u0.length];
	    double[] gamma = new double[u0.length];
	    for (int i = 0; i < u0.length; i++)
	      zeros[i] = 0;
	    callbacks().Toms731_LHB(ctxt, t0, zeros, zeros, beta, gamma);
	    for (int i = 0; i < u0.length; i++) {
	      if (beta[i] == 0)
	        u0[i][0] = uf[i][0] = -gamma[i];
	    }
	    callbacks().Toms731_RHB(ctxt, t0, zeros, zeros, beta, gamma);
	    for (int i = 0; i < u0.length; i++) {
	      if (beta[i] == 0)
	        u0[i][u0[0].length-1] = uf[i][u0[0].length-1] = -gamma[i];
	    }
	
	    //double[][] uxxf = new double[u0.length][u0[0].length];
	    oneTStep(ctxt, t0, tfinal, xgrid, u0, uf);
  	}

	// one time-step
	public void oneTStep(RTContext ctxt,
	double t, double tend, double[] xgrid, 
	double[][] u0, double[][] u) throws Xcept {

	    neqn = u.length;
	    ngrid = xgrid.length;
	    double[] y = new double[(neqn+1)*ngrid];
	    double[] movingGrid = new double[ngrid];
	    double[][] movingY = new double[neqn][ngrid];

	    for (int i = 0; i < neqn; i++)
	      for (int j = 0; j < ngrid; j++)
	        y[i+j*(neqn+1)] = u[i][j];

	    int idid = jtoms731(ctxt.threadInx, ctxt, neqn, t, tend,
	    	ngrid, xgrid[0], xgrid[ngrid-1], u0, y, callbacks());

	    if (idid < 0) if (idid < 0) throw new Xcept (
		solverName + " faild: idid=" + idid);

	    InterpolationSpline[] spline = 
		new InterpolationSpline[neqn];
	    for (int i = 0; i < ngrid; i++)
	      movingGrid[i] = y[(neqn+1)*(i+1)-1];
	    for (int j = 0; j < neqn; j++) {
	      for (int i = 0; i < ngrid; i++)
	        movingY[j][i] = y[(neqn+1)*i+j];
	      spline[j] = new InterpolationSpline(
		movingGrid, movingY[j], 0, 0);
	    }

	    for (int i = 0; i < neqn; i++) {
	      for (int j = 0; j < ngrid; j++) {
	        if (j == 0 || j == ngrid-1)
	          u[i][j] = y[i+j*(neqn+1)];
	        else
	          u[i][j] = spline[i].splint(
		    movingGrid, movingY[i], xgrid[j]);
	      }
	    }
	}

	// native call
	static native int jtoms731(
	    int threadInx, RTContext ctxt, 
	    int neqn, double t, double tend,
            int ngrid, double xmin, double xmax,
            double[][] u0, double[] y, PDE1Callbacks h);
}
