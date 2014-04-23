/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// function derived from fixed sample data

package JSim.fgen;

import JSim.util.*;
import JSim.data.*;


public class FgenData extends FgenSlave {
	private Data data;

	// constructor
	public FgenData(FgenMaster f, String n, FgenContext ctxt) 
	throws Xcept { 
	    super(f,n,ctxt);
	}

	// dimensionality
	public int ndim() { 
	    try {
		return getData().ndim(); 
	    } catch (Xcept e) {
		return 1; // HACK!!!
	    }
	}

	// initialize at beginning of run
	public void init() throws Xcept {
	    super.init();
	    data = getData();
	}

	// function value for particular domain values
	public double realVal(double[] vals) throws Xcept {
	    return data.realVal(vals);
	}

	// retrieve source data 
	private Data getData() throws Xcept {
	    Data data = dataVal("name");
	    if (data == null) throw new Xcept(this,
		"data source is null");
	    return data;
	}
	    
}


