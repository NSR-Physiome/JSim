/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Int control parameter for project

package JSim.project;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.NamedVal;

public class IntControl extends Control {
	protected int value;  // internal value
	protected int defaultValue;  // default value

	// constructor
	public IntControl(PNamed p, String n, int d) 
	throws Xcept {
	    super(p, n);
	    value = defaultValue = d;
	}

	// specific methods
	public int dataType() { return Expr.REAL; }
	public double realVal() { return value; }
	public int val() { return value; }
	public String stringVal() { return "" + value; }
	public String stringDef() { return "" + defaultValue; }
	public void setVal(String s) throws Xcept { 
	    value = Util.toInt(s);
	    update();
	}
	public void setVal(int i) throws Xcept {
	    value = i;
	    update();
	}

	// NamedVal with given name n
  	public NamedVal namedVal(String n) throws Xcept {
	    return NamedVal.create(n, value);
	}
}

