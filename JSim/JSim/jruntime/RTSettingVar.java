/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Solver/Fgen setting variable

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public class RTSettingVar extends NamedExpr {
	private RTModel model;
	private String name;
	private int dataType;

	// constructor
	public RTSettingVar(RTModel model, String name, int dataType) { 
	    this.model = model;
	    this.name = name;
	    this.dataType = dataType;
	}

	// value query
	public double realVal(Context ctxt) throws Xcept {
	    return namedVal(ctxt).realVal();
	}
	public boolean boolVal(Context ctxt) throws Xcept {
	    return namedVal(ctxt).boolVal();
	}
 	public String stringVal(Context ctxt) throws Xcept {
	    return namedVal(ctxt).stringVal();
	}
    
	// find setting
	private NamedVal namedVal(Context ctxt) throws Xcept {
	    if (! (ctxt instanceof RTContext)) throw new Xcept(
	    	"RTSettingVar.namedVal requires RTContext");
	    NamedVal nval = ((RTContext) ctxt).namedVal(name);
	    if (nval == null) throw new Xcept(
	    	"RTSettingVar " + name + ": setting not found");
	    return nval;
	}

	// simple query
	public String name() { return name; }
	public String diagInfo() { return "Setting " + name; }
	public String toString() { return name; }
	public String toString(Context ctxt) { 
	    return ctxt.newName(this); 
	}
	public Expr takeDomDeriv(NamedExpr e) { return zero; }
	public Expr expandDeriv() { return this; }
	public Expr unitCorrect() { return this; }
	public Unit unit() { return null; }
	public void addDomains(Expr.List list) {  }
	public boolean sameAs(Expr e) { 
	    if (! (e instanceof RTSettingVar)) return false;
	    return name.equals(((RTSettingVar) e).name());
	}
	public int dataType() { return dataType; }
}
