/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time external function

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import java.util.ArrayList;

abstract public class RTXFunc implements DiagInfo {
	public RTModel model;
	public String name;

	// constructor
	public RTXFunc(RTModel m, String n) throws Xcept {
	    model = m;
	    name = n;
	}

	// query info
	public static Info getInfo(Expr.List args) throws Xcept {
	    return null;
	}

	// calculate 
	public double realCalculate(RealNData[] args) throws Xcept {
	    throw new Xcept(this, "realCalculate() not implemented");
	}
	public void voidCalculate(RealNData[] args) throws Xcept {
	    throw new Xcept(this, "voidCalculate() not implemented");
	}

	// query
	public String name() { return name; }
	public String diagInfo() { return "Function " + name; }

	// model-generated warning to user
  	public void warning(String msg) {
	    model.warning(msg);
	}

	// RTXFunc.Info class
	public static class Info {
	    public int ninputs;
	    public int dataType;
	}
}

