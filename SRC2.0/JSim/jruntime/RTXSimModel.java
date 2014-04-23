/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time XSim model

package JSim.jruntime;

import JSim.util.*;

abstract public class RTXSimModel {
	protected RTModel rtmodel;

	// constructor
	public RTXSimModel() {	}

	// set model
	public void setModel(RTModel m) {
	    rtmodel = m;
	}

	// public wrappers
	public int pSize() { 
	    return xsimPSize(); 
	}
	public void setP(int loc, double v) { 
	    xsimSetP(loc, v);
	}
	public double getP(int loc) { 
	    return xsimGetP(loc);
	}
	public void init() throws Xcept { 
	    xsimini();
	}
	public void loop() throws Xcept { 
	    xsimlop();
	}
	public void comp() throws Xcept { 
	    xsimend();
	}

	// array setP/getP utilities
	public double[] getP(int loc, int n, int incr) {
	    double[] d = new double[n];
	    for (int i=0; i<n; i++) {
		d[i] = xsimGetP(loc);
		loc += incr;
	    }
	    return d;
	}
	public void setP(int loc, int n, int incr, double[] d) {
	    for (int i=0; i<n; i++) {
		xsimSetP(loc, d[i]);
		loc += incr;
	    }
	}

	// warning from model
	public void warning(String s) {
	    if (rtmodel != null && s != null) 
		rtmodel.warning(s);
	}

	// fatal error from model
	public void fatal(String s) throws Xcept {
	    throw new Xcept(s);
	}

	// model-specific calls calls
	protected abstract int xsimPSize();
	protected abstract void xsimSetP(int loc, double v);
	protected abstract double xsimGetP(int loc);
	protected abstract void xsimini() throws Xcept;
	protected abstract void xsimlop() throws Xcept;
	protected abstract void xsimend() throws Xcept;
}


