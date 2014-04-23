/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Boolean control parameter for project

package JSim.project;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.NamedVal;

public class BooleanControl extends Control {
	private boolean value;  // internal value
	private boolean defaultValue;  // internal value
	private String[] labels; // true and false labels

	// constructor
	public BooleanControl(PNamed p, String n, boolean d,
	String[] l) throws Xcept {
	    super(p, n);
	    value = defaultValue = d;
	    labels = l;
	    if (labels.length != 2) throw new Xcept(this,
		"BooleanControl must have exactly 2 labels");
	}

	public BooleanControl(PNamed p, String n, boolean d) throws Xcept {
	    this(p, n, d, new String[] { "true", "false" });
	}

	// specific methods
	public int dataType() { return Expr.BOOLEAN; }
	public boolean boolVal() { return value; }
	public boolean val() { return value; }
	public String stringVal() { return labels[value?0:1]; }
	public String stringVal(boolean b) { return labels[b?0:1]; }
	public String stringDef() { return labels[defaultValue?0:1]; }
	public void setVal(String s) throws Xcept { 
	    if (s.equals(labels[0])) {
		value = true;
		update();
	    } else if (s.equals(labels[1])) {
		value = false;
		update();
	    } else
		throw new Xcept(this, "Invalid value \"" +
		    s + "\""
		+ " [0]=" + labels[0] + " [1]= " + labels[1]);
	}
	public void setVal(boolean b) throws Xcept {
	    value = b;
	    update();
	}

	// NamedVal with given name n
  	public NamedVal namedVal(String n) throws Xcept {
	    return NamedVal.create(n, value);
	}

}

