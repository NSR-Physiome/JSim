/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Run-time variable

package JSim.jruntime;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;
import JSim.aserver.*; 
import java.util.ArrayList;

abstract public class RTVar extends ASVar {
	protected RTModel model;
	protected String name;
	protected Unit unit;	// model-defined unit
	protected Expr defAssign;	// default value 
	protected Expr assign;	// assigned value (next run)
	private boolean isPrivate;
	private int phase;  // inputs=1, outputs >1, 0 unsolved private
	public final static int INPUT = 1;
	private int varID; // index to model.vars
	protected StringList domainNames;

	// static constructor
	public RTVar(RTModel m, String n, String u, int ph) 
	throws Xcept {
	    model = m;
	    name = n;
	    unit = model.parseUnit(u);
	    phase = ph;
	    isPrivate = false;
	    varID = model.vars.size();
	    model.add(this);
	}

	// set to default assignment & unit
	public void setDefault() throws Xcept {
	    if (defAssign != null) setAssign(defAssign.toString());
	}
	public void setDefault(String n) throws Xcept {
	    Expr e = model.parseExpr(n).simplify();
	    checkAssign(e);
	    defAssign = e;
	    assign = e;
	}

	// NEW assignments
	public void setAssign(String s) throws Xcept {
	    if (s == null) {
	        assign = null;
		return;
	    }
	    try {
	    	Expr expr = model.parseExpr(s);
	    	checkAssign(expr);
	    	assign = expr;
	    } catch (Exception e) {
	    	assign = null;
	        throw Xcept.wrap(e);
	    }
	}	    
	final public String getAssign() { 
	    return (assign == null) ? 
	        null : assign.toString();
	}
	final public String getDefault() { 
	    return (defAssign == null) ? 
	        null : defAssign.toString();
	}

	// set other var properties 
	public void setPrivate() { isPrivate = true; }
	public void setName(String n) { name = n; }

	// query
	final public String toString() { return name; }
	final public Unit unit() {  return unit; }  
	final public Expr unitCorrect() { return this; }
	final public String name() { return name; }
	final public boolean isInput() { return phase == INPUT; }
	final public int phase() { return phase; }
	final public boolean isPrivate() { return isPrivate; }
	final public int dataType() { return Expr.REAL; }
	final public int varID() { return varID; } 
	public RTVar.List domainList() {
	    RTVar.List list = new RTVar.List(ndim());
	    addDomains(list);
	    return list;
	}
	final public boolean sameAs(Expr e) {
	    return (e == this);
	} 
	public String toString(Context ctxt) {
	    return ctxt.lang.newName(name);
	}
	public void setUnit(String s) throws Xcept {
	    throw new Xcept(this, "setUnit() not implemented");
	}

	// final val from latest run
	final public double finalRealVal() throws Xcept {
	    RTDataStore store = model.finalStore();
	    if (store == null) return Double.NaN;
	    Data data = store.data(this);
	    if (data == null || data.nsamples()<1)
		return Double.NaN;
	    return data.realVal(data.nsamples()-1);
	}

	// check input var assignble
	final public void checkAssign(Expr e) throws Xcept {
	    checkAssign(e, model.unitCorrect);
	}
	private void checkAssign(Expr e, boolean unitCorrect) throws Xcept {
	    if (! isInput()) throw new Xcept(this,
		"User may not assign values to output variables.");

	    // check vars for dependencies
	    ASVar.List vlist = new ASVar.List(1);
	    for (int i=0; i<ndim(); i++)
		vlist.add(domain(i));
	    ASVar.List elist = new ASVar.List(1);
	    e.addDomains(elist);
	    if (! vlist.containSet(elist)) throw new Xcept(this, e,
		"Variable assignment invalid due to domain conflict");
	    Expr.List xlist = new Expr.List(1);
	    e.addNamedExpr(xlist);
	    for (int i=0; i<xlist.size(); i++) {		
		if (! (xlist.expr(i) instanceof ASVar)) continue;
		ASVar v = (ASVar) xlist.expr(i);
		if (v.isInput()) continue;
		if (v.ndim() == 0) continue;
		if (v.isDomain()) continue;
		throw new Xcept(this, v,
		    "May not calculate input variable from output variables");
	    }

	    // check unit consistency
	    if (unitCorrect) {
		Unit u = e.unitCorrect().unit();
		if (! Unit.compatible(u, unit())) 
		   throw new Xcept(this, e,
			"Assignment units are incompatible");
	    }
	}	    

	// list of vars
	public static class List extends ASVar.List {
	    public List(int i) { super(i); }
	    public RTVar var(int i) { return (RTVar) get(i); }
	}

	// named list of vars
	public static class NList extends NamedList {
            public NList(int i) { super(i); }
            public RTVar var(int i) { return (RTVar) get(i); }
            public RTVar var(String n) { return (RTVar) getByName(n); }
        }	    
}

