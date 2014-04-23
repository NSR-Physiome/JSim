/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// String control parameter for project

package JSim.project;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.NamedVal;

public class StringControl extends Control {
	protected String value;  // internal value, never null!!!
	protected boolean valid; // verified internally
	protected String validMsg; // message from validation	
	protected String defaultValue; // default
	protected StringList fixedPickList;

	// constructor
	public StringControl(PNamed p, String n) throws Xcept {
	    this(p, n, null, null);
	}
	public StringControl(PNamed p, String n, String d) throws Xcept {
	    this (p, n, d, null);
	}
	public StringControl(PNamed p, String n, String d,
	StringList pickList) throws Xcept {
	    super(p, n);
	    if (d == null) d = "";
	    value = defaultValue = d;
	    update();
	    fixedPickList = pickList;
	}

	// query
	public int dataType() { return Expr.STRING; }
	public String stringVal() { return value; }
	public String stringDef() { return ""; }
	public String val() { return value; }
	public boolean valid() { return valid; }
	public String validMsg() { return validMsg; }
	public boolean  isBlank() { return Util.isBlank(value); }
	public StringList pickList() { return fixedPickList; }

	// NamedVal with given name n
  	public NamedVal namedVal(String n) throws Xcept {
	    return NamedVal.create(n, value);
	}

	// set value
	public void setVal(String s) throws Xcept { 
	    if (s == null) s = "";
	    value = s; 
	    update();
	}

	// set blank
	public void setBlank() throws Xcept { 
	    value = ""; 
	    update();
	}

	// call when value is changed
	public void update() throws Xcept {
	    super.update();
	    revalidate();
	}

	// recalculate valid flag
	public void revalidate() { valid = true; }
}

