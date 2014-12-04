/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// component property (e.g. v.desc, v.help)

package JSim.mml; 

import JSim.util.*;
import JSim.expr.*;

import java.util.ArrayList;

public class CompProp extends NamedExpr {
	protected Comp comp;	// attached to this
	protected String name;	// name of property
	protected Expr expr; 	// property value
	protected int dataType; // expr data type 

	// constructor for package only
	protected CompProp(Comp c, String n, int d) throws Xcept {
	    comp = c;
	    name = n;
	    dataType = d;
	    // c.addProp(this); 
	}

	// set value
	public void setVal(Expr e) throws Xcept {
	    e = e.simplify();
	    if (e.dataType() != dataType) throw new Xcept(this,
		"property requires " + dataTypeName(dataType) +
		" value");
	    if (expr != null && !expr.sameAs(e)) 
		throw new Xcept(this,
		    "conflicting values for property");
	    expr = e;
	}

	// set value based on statement
	public void setStmt(Expr e) throws Xcept {
	    if (! (e instanceof CompareExpr)) throw new Xcept(this,
		"illegal property usage (equation expected)");
	    CompareExpr ce = (CompareExpr) e;
	    if (ce.op() != ce.EQ) throw new Xcept(this,
		"illegal property usage (equals sign expected)");
	    if (ce.arg(0) != this) throw new Xcept(this,
		"illegal property usage (LHS)");
	    setVal(ce.arg(1));
	}

	// query
	public String name() { return name; }
	public int dataType() { return dataType; }
	public boolean boolVal(Context ctxt) throws Xcept { 
	    return expr.boolVal(ctxt);
	}
	public double realVal(Context ctxt) throws Xcept { 
	    return expr.realVal(ctxt);
	}
	public String stringVal(Context ctxt) throws Xcept { 
	    if (expr == null) return null;
	    return expr.stringVal(ctxt);
	}
	public boolean isSet() { return expr != null; }
	public String toString() { 
	    return comp.name() + "." + name;
	}
	public boolean isConst() { return true; }
	public String toString(Context ctxt) { return toString(); }	
	public ConstExpr cons() throws Xcept { 
	    return expr.cons(); 
	} 
	public Expr simplify() { return this; }
	public Unit unit() {
	    return (expr != null) ? expr.unit() : null;
	}
	public boolean sameAs(Expr expr) {
	    return expr == this;
	}

	// update
	public Expr unitCorrect() { return this; }
	public void addDomains(Expr.List list) { }
	public void addNamedExpr(Expr.List list) { 
	    list.addUniq(this);
	}

	// derivate manipulations
	public Expr takeDomDeriv(NamedExpr t) {
	    return zero;
	}
	public Expr expandDeriv() { return this; }

	// convenience
	public void setVal(String s) throws Xcept {
	    setVal(new StringConst(s)); 
	}
	public void setVal(boolean b) throws Xcept {
	    setVal(new BoolConst(b));
	}
	public void setVal(double d) throws Xcept {
	    setVal(new RealConst(d));
	}

	// CompProp.List
	public static class List extends NamedExpr.List {
	    public List(int n) { super(n); }
	    public CompProp prop(int i) { return (CompProp) get(i); }
	}
}
