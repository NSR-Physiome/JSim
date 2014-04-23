/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Function-generator slave calculation

package JSim.fgen;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;


abstract public class FgenSlave implements DiagInfo {
	protected FgenMaster funcGen;
	protected String name;
	protected FgenContext ctxt;
	protected NamedExpr domains[];

	// constructor
	public FgenSlave(FgenMaster f, String n, FgenContext c) throws Xcept {
	    funcGen = f;
	    name = n;
	    ctxt = c;
	    init();
	}

	// initialize at beginning of run
	public void init() throws Xcept {
	    domains = new NamedExpr[ndim()];
	    for (int i=0; i<ndim(); i++) {
		String xname = 
		    funcGen.namedVal("domain" + i, ctxt).stringVal();
		domains[i] = ctxt.domain(xname);
	    }
	}

	// query current value
	public double realVal(FgenContext ctxt) throws Xcept {
	    double[] xvals = new double[ndim()];
	    for (int i=0; i<xvals.length; i++) 
		xvals[i] = domains[i].realVal(ctxt);
	    return realVal(xvals);
	}

	// query data
	public RealNData data() throws Xcept {
	    if (ndim() != 1) throw new Xcept(this,
	    	"Only 1D data currently available");
	    RegularGridData grid = domainGrid(0);
	    RealNData data = new RealNData(funcGen.name(), null,
		new GridData[] { grid });
	    for (int t=0; t<grid.ct(); t++) {
		double tval = grid.realVal(t);
		double val = realVal(new double[] { tval });
		data.set(t, val);
	    }
	    return data;
	}	    


	// query current grid
	protected RegularGridData domainGrid(int n) throws Xcept {
	    if (n>=domains.length) throw new Xcept(this,
		"Invalid domain #");
	    GridData grid = ctxt.gdata(domains[n]);
	    if (grid == null) throw new Xcept(this,
		"Domain grid[" + n + "] is null");
	    if (! (grid instanceof RegularGridData))
		throw new Xcept(this, "Irregular grids not supported");
	    return (RegularGridData) grid;
	}

	// dimensionality
	abstract public int ndim();

	// function value for particular domain values
	abstract public double realVal(double[] vals) throws Xcept;

	// diaginfo
	public String diagInfo() {
	    return "Function generator " + getClass().getName();
	}

	// param query
	public final NamedVal namedVal(String n) throws Xcept {
	    return funcGen.namedVal(name + "." + n, ctxt);
	}
	public final double realVal(String name) throws Xcept {
	    return namedVal(name).realVal();
	}
	public final int intVal(String name) throws Xcept {
	    return namedVal(name).intVal();
	}
	public final boolean boolVal(String name) throws Xcept {
	    return namedVal(name).boolVal();
	}
	public final String stringVal(String name) throws Xcept {
	    return namedVal(name).stringVal();
	}
	public final Data dataVal(String name) throws Xcept {
	    return namedVal(name).dataVal();
	}

}


