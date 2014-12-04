/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// argument to external run-time function

package JSim.jruntime;

import JSim.util.*;
import JSim.data.*;
import java.util.ArrayList;

public class RTXFuncArg implements DiagInfo {
	private RTXFuncCall funcCall; // to this call
	private int argn;		// argument # for this call
	private RTRealDomain argDoms[]; // arg domains apparent to RTXFunc
	private RTVar v; // if simple variable
	private boolean isInput;    // if variable,  is input?

	// variable constructor
	public RTXFuncArg(RTXFuncCall fc, RTRealDomain[] ad, RTVar vv, 
	boolean isIn)  {
	    funcCall = fc;
	    argDoms = ad;
	    v = vv;
	    isInput = isIn;
	    RTVar.List list = v.domainList();
	    argn = funcCall.args.size();
	    funcCall.addArg(this);
	}

	// expression constructor
	public RTXFuncArg(RTXFuncCall fc, RTRealDomain[] ad) {
	    funcCall = fc;
	    argDoms = ad;
	    v = null;
	    isInput = true;
	    argn = funcCall.args.size();
	    funcCall.addArg(this);
	}

	// expression calculation - override when v == null
	protected double getRealVal(RTContext ctxt) throws Xcept {
	    throw new Xcept(
		"RTXFuncArg.getRealVal() not implemented");
	}

	// public query
	public int ndim() { return argDoms.length; }
	public boolean isInput() { return isInput; }
	public String diagInfo() { 
	    return funcCall.name() + " arg" + argn;
	}

	// create ndata just before function call
	protected final RealNData loadData(RTContext ctxt) throws Xcept {

	    // save state, create NData
	    //   some current domains settings must be preserved
	    int[] saveState = new int[ndim()];
	    GridData[] grids = new GridData[ndim()];
	    for (int i=0; i<ndim(); i++) {
		saveState[i] = ctxt.getCState(argDoms[i]);
		grids[i] = ctxt.grid(argDoms[i]);
	    }

	    // fill in input data
	    RealNData ndata = new RealNData("arg" + argn, null, grids);
	    if (!isInput()) return ndata;
	    for (int i=0; i<ndata.nsamples(); i++) {
		int[] gpos = ndata.gridPos(i);
		for (int j=0; j<ndim(); j++)
		    ctxt.setDInx(argDoms[j], gpos[j]);
		double val = (v==null) ? 
		    getRealVal(ctxt) : v.realVal(ctxt);
		ndata.set(i, val);
	    }

	    // restore state & return
	    for (int i=0; i<ndim(); i++) 
		ctxt.setCState(argDoms[i], saveState[i]);
	    return ndata;
	}

	// unload output back into v
	protected void unloadData(RTContext ctxt, RealNData ndata) 
	throws Xcept {
	    if (isInput()) return;
	    if (! (v instanceof RTRealNVar)) throw new Xcept(this, v,
		"only RTRealNVar's supported in unloadData()");
	    RTRealNVar nv = (RTRealNVar) v;
	    
	    // save state, create NData
	    int[] saveState = new int[ndim()];
	    for (int i=0; i<ndim(); i++) 
		saveState[i] = ctxt.getCState(argDoms[i]);

	    // unload output data
	    for (int i=0; i<ndata.nsamples(); i++) {
		int[] gpos = ndata.gridPos(i);
		for (int j=0; j<ndim(); j++)
		    ctxt.setDInx(argDoms[j], gpos[j]);
		ctxt.set(nv, ndata.realVal(i));
	    }

	    // restore state & return
	    for (int i=0; i<ndim(); i++) 
		ctxt.setCState(argDoms[i], saveState[i]);
	}
	    
	// list of RTXFuncArgs
	public static class List extends ArrayList<RTXFuncArg> {
	    public List(int i) { super(i); }
	    public RTXFuncArg arg(int i) { return (RTXFuncArg) get(i); }
	}

}
