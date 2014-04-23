/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time external function call

package JSim.jruntime;

import JSim.util.*;
import JSim.data.*;
import java.util.ArrayList;

public class RTXFuncCall implements DiagInfo {
	private RTXFunc func;
	protected RTXFuncArg.List args;

	// constructor
	public RTXFuncCall(RTXFunc f) {
	    func = f;
	    args = new RTXFuncArg.List(4);
	}

	// add argument
	protected void addArg(RTXFuncArg arg) {
	    args.add(arg);
	}

	// query
	public String name() { return func.name(); }
	public String diagInfo() { return "Function call " + func.name(); }
	public RTXFunc func() { return func; }

	// calculate
	private RealNData[] makeArgs(RTContext ctxt) throws Xcept {
	    RealNData[] nargs = new RealNData[args.size()];
	    for (int i=0; i<args.size(); i++) 
		nargs[i] = args.arg(i).loadData(ctxt);
	    return nargs;
	}
	public double realVal(RTContext ctxt) throws Xcept {
	    return func.realCalculate(makeArgs(ctxt));
	} 
	public void voidVal(RTContext ctxt) throws Xcept {
	    RealNData[] nargs = makeArgs(ctxt);
	    func.voidCalculate(nargs);
	    for (int i=0; i<args.size(); i++) 
		args.arg(i).unloadData(ctxt, nargs[i]);
	} 
}

