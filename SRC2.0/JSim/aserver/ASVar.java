/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/
// Project model var,  may be local RTVar or client stub

package JSim.aserver;

import JSim.util.*;
import JSim.expr.*;
import JSim.data.*;

public abstract class ASVar extends NamedExpr implements ASQuery {

	// is input variable
	abstract public boolean isInput();

	// is domain variable
	abstract public boolean isDomain();

	// get domain variable
	abstract public ASVar domain(int i);

	// dimension
	abstract public int ndim();

	// unit string
	public String unitString() {
	    if (unit() == null) return "";
	    return unit().pubName();
	}	
	
	// get final value from model run
	abstract public double finalRealVal() throws Xcept;

	// set input var assignment
	abstract public void setAssign(String s) throws Xcept;

	// get input var assignment,  or null
	abstract public String getAssign() throws Xcept;

	// get default input var assignment,  or null
	abstract public String getDefault() throws Xcept;

	// choice-associated labels and values
	public String[] labels() { return null; }
	public String[] labelValues() { return null; }

	// add domains
	public void addDomains(Expr.List list) {
	    for (int i=0; i<ndim(); i++) {
		ASVar x = domain(i);
		if (x != null) list.addUniq(x);
	    }
	} 

	// full name with domains
	public String fullName() {
	    if (ndim() == 0) return name();
	    StringBuffer s = new StringBuffer(name() + "(");
	    for (int i=0; i<ndim(); i++) {
		if (i>0) s = s.append(",");
		s.append(domain(i).name());
	    }
	    s.append(")");
	    return s.toString();
	}

	// same?
	public boolean sameAs(Expr e) { return (e == this); } 

	// simplify
	public Expr simplify() { return this; }

	// useless/unimplements Expr stuff
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, "takeDeriv not implemented");
	} 
	public Expr expandDeriv() throws Xcept {
	    throw new Xcept(this, "expandDeriv not implemented");
	} 


	// list of vars
	public static class List extends NamedExpr.List {
	    public List(int i) { super(i); }
	    public ASVar asvar(int i) { return (ASVar) get(i); }
	}

	// named list of vars
	public static class NList extends NamedList {
	    public NList(int i) { super(i); }
	    public ASVar asvar(int i) { return (ASVar) get(i); }
	    public ASVar asvar(String n) { return (ASVar) getByName(n); }
	}

}
