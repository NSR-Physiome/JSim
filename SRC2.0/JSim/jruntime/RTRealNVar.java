/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// n-dimensional real variable

package JSim.jruntime;
import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import java.util.Random;

public class RTRealNVar extends RTNVar {

	// constructor
	public RTRealNVar(RTModel m, String n, String u, int ph,
	RTRealDomain[] d) throws Xcept {
	    super(m, n, u, ph, d);
	}

	// real value
	public double realVal(Context ctxt0) throws Xcept {
	    if (! (ctxt0 instanceof RTContext)) throw new Xcept(this,
		"realVal(ctxt) requires RTContext");
	    RTContext ctxt = (RTContext) ctxt0;
	    return ctxt.realVal(this); 
	}

	// make data array
	protected RealNData makeData(RTContext ctxt) throws Xcept {
	    Util.verbose("\t" + this + " makeData");
	    GridData[] grids = new GridData[ndim()];
	    for (int i=0; i<ndim(); i++)
		grids[i] = ctxt.grid(doms[i]);
	    return new RealNData(name(), unit(), grids);
	}

	// calculate assigned input
	//   pass random from old context to allow repeatable assigns ??? 
	protected RealNData calcAssign(RTDataStore store, Expr expr,
	Random random) 
	throws Xcept { 
	    RTContext ctxt = new RTContext(model, 
	    	store.runCtxt().threadInx, 1, store, false);
	    ctxt.random = random; // ???
	    if (expr == null) expr = assign;
	    Data data = model.getData(ctxt, expr, doms, unit());
	    if (data.ndim() != ndim()) throw new Xcept(this,
		"calcAssign() returned data dimension " + data.ndim()
		+ " s/b " + ndim());
	    RealNData ndata = null;
	    if (data instanceof RealNData)
		ndata = ((RealNData) data);
	    else if (data instanceof GridData)
		ndata = new RealNData((GridData) data);
	    else throw new Xcept(this,
		"Assign returned unsupported data type");
	    ndata.setDesc(name());
	    return ndata;
	}
}

