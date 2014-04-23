/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Control for assigned value of model parameter

package JSim.project; import JSim.aserver.*;

import JSim.util.*;
import JSim.expr.*;

public class AssignControl extends StringControl {
	private ASVar v;	// variable to assign

	// constructor
	public AssignControl(PNamed p, String n, ASVar vv) 
	throws Xcept {
	    super(p, n);
	    v = vv;
	    setVal(v.getAssign());
	}

	// set value from optimizer best value
	// don't revalidate: causes sync issue with server setAssign()
	protected void setValFromOptim(double v) throws Xcept {
	    value = PrettyFormat.sformat(v, 4);
	    valid = true;
	    parent.childChanged(this);
	}
	
	// call when value is changed
	public void update() throws Xcept {
	    super.update();
	}

	// recalculate valid flag
	public void revalidate() {
	    valid = false;
	    validMsg = null;
	    if (isBlank()) return;
	    try {
//		v.setAssign(null);
		v.setAssign(value);
		valid=true;
	    } catch (Xcept e) {
		validMsg = e.cleanMessage();
	    }
	}

	// query
	public ASVar asvar() { return v; }	    
	public String stringDef() {
	    try {
	    	String s = v.getDefault();
	        return (s == null) ? "" : s;
	    } catch (Xcept e) {
	    	return "";
	    }
	}

	// ControlVars
	public int dataType() {
	    return v.dataType();
	}
	public Unit unit() {
	    return v.unit();
	}
	public double realVal() throws Xcept {
	    if (dataType() != Expr.REAL) throw new Xcept(this,
		"Real valued assign expected");
	    return isBlank() ? Double.NaN : Util.toDouble(value);
	}
	public boolean boolVal() throws Xcept {
	    if (dataType() != Expr.BOOLEAN) throw new Xcept(this,
		"Boolean valued assign expected");
	    if (value.equals("true")) return true;
	    return false;
	}

	// picklist
	public StringList pickList() {
	    if (v.labels() == null) return null;
	    return new StringList(v.labels());
	}
}

