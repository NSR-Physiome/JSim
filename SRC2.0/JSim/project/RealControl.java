/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Real control parameter for project

package JSim.project;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.NamedVal;

public class RealControl extends Control {
	private double value;  // internal value
	private double defaultValue;  // default value

	// constructor
	public RealControl(PNamed p, String n, double d) 
	throws Xcept {
	    super(p, n);
	    value = defaultValue = d;
	}

	// specific methods
	public int dataType() { return Expr.REAL; }
	public double realVal() { return value; }
	public double val() { return value; }
	public String stringVal() { return Util.pretty(value); }
	public String stringDef() { return Util.pretty(defaultValue); }
	public void setVal(String s) throws Xcept { 
	    value = Util.toDouble(s);
	    update();
	}
	public void setVal(double v) throws Xcept {
	    value = v;
	    update();
	}

	// NamedVal with given name n
  	public NamedVal namedVal(String n) throws Xcept {
	    return NamedVal.create(n, value);
	}
}

