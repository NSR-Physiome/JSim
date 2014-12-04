/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// test sensitivity matrix

package JSim.tests; 

import JSim.util.*;
import JSim.data.*;

public class TestSensMat {
	private GridData[] tgrid;

	// constructor
	public TestSensMat(double pct) throws Xcept {
	    
	    // model parms
	    int nx = 2;
	    double a = 1;
	    double b = Math.PI/2;
	    double delta = 0.01;
	    double x[] = new double[] { a, b };
	    double xdelta[] = new double[] { delta, delta };
	    
	    // alloc data
	    int tct = 4;
	    double[] t = new double[tct];
	    double[] y = new double[tct];
	    double[] pw = new double[tct];
	    double[] h = new double[tct];
	    double[][] hk = new double[nx][tct];
	    double[] cw = new double[1];
            pw[0]=6.0/20.0;
            pw[1]=9.0/20.0;
            pw[2]=5.0/44.0;
            pw[3]=6.0/44.0;
	    // calc data
	    for (int i=0; i<tct; i++) {
	        t[i] = i * 1;
		y[i] = f1(t[i], a, b); 
                double r=-1.0+5.4979453002;
                double s=-Math.PI/2-1.537749546;
		h[i] = f(t[i], a+r, b+s);
		hk[0][i] = f(t[i], a+r+delta, b+s);	
		hk[1][i] = f(t[i], a+r, b+s+delta);	
	    }	    
	    cw[0] =  1;
	    tgrid = new GridData[] { new IrregularGridData("t", null, t) };

	    // calc matrix
	    SensMatrix mat = new SensMatrix(x, xdelta, 
	    	list(y), list(h), lists(hk), list(pw), cw);
            double[][] s = mat.getSensMatrix();
            int nd = s.length;
	    double[][] m = mat.getCovMatrix();
  	    double[] conf = mat.getConfLimits(pct);

	    // print
	    System.out.println("Sens mat");
	    for (int i=0; i<nd; i++) {
	    	for (int j=0; j<nx; j++) 
		    System.out.print("\t" + s[i][j]);
		System.out.println("");
	    }
	    System.out.println("Cov mat");
	    for (int i=0; i<nx; i++) {
	    	for (int j=0; j<nx; j++) 
		    System.out.print("\t" + m[i][j]);
		System.out.println("");
	    }
  	    System.out.println("Conf limits");
	    for (int i=0; i<nx; i++) {
	    	System.out.println("\t" + conf[i]);	    
	    }
        }
	// model function
	private double f(double t, double a, double b) {
	    return a*t+b;
        }
	private double f1(double t, double a, double b) {
	    return a*t+b*t*t;
	}

	// create Data.List from array
	private Data.List list(double[] arr) throws Xcept {
	    return new Data.List(new Data[] { data(arr) });
	}
	private Data.List[] lists(double[][] arr) throws Xcept {
	    int n = arr.length;
	    Data.List[] larr = new Data.List[n];
	    for (int i=0; i<n; i++)
	    	larr[i] = list(arr[i]);
	    return larr;
	}
	private Data data(double[] arr) throws Xcept {
	    return new RealNData("data", null, tgrid, arr);
	}

	// mainline
	public static void main(String[] args) throws Xcept {
	    new TestSensMat(Util.toDouble(args[0]));
	}
}
