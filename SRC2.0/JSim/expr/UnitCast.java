/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Cast expression to particular unit

package JSim.expr;
import JSim.util.*;

public class UnitCast extends Expr {
	public Expr expr;
	public Unit unit;
	private double factor;

	// constructor
	public UnitCast(Expr e, Unit u) throws Xcept {
	    super();
	    expr = e;
	    unit = u;
	    if (expr.dataType() != REAL) throw new Xcept(
	    	"Expr " + e + " must have datatype=REAL to use unit cast");
	    factor = 1;
	}

	// simple query
	public int dataType() { return expr.dataType(); }
	public Expr expr() { return expr; }
	public Unit unit() { return unit; }
	public double factor() { return factor; }
	public boolean isDomain() { return expr.isDomain(); } //???
	public double realVal(Context ctxt) throws Xcept {
	    return factor * expr.realVal(ctxt); 
	}
	public String diagInfo() { 
	    return "UnitCast " + expr + " -> " + unit.pubName();
	}
	public String toString(Context ctxt) {
	    return ctxt.unitCast(this);
	}
	public String toString() {
	    if (factor == 1) return expr.toString();
	    return "(" + expr + "*" + factor + ")";
	}
	
	// unit correct
	public Expr unitCorrect() throws Xcept {
	    Expr nexpr = expr.unitCorrect();
	    Unit u = nexpr.unit();
	    if (! Unit.compatible(unit, u)) throw new Xcept(
	    	unit, u, "Incompatible unit cast");
	    UnitCast cast = new UnitCast(nexpr, unit);
	    cast.factor = Unit.convertFactor(unit, u);
	    return cast;
	}

	// test same
	public boolean sameAs(Expr e) {
	    try {
	    	UnitCast ucx = (UnitCast) e;
		return expr.sameAs(ucx.expr) 
		    && Unit.same(unit, ucx.unit);
	    } catch (ClassCastException x) {
		return false;
	    }
	}
	
	// simplify
	public Expr simplify() { return this; }
	
	// derivatives
	public Expr expandDeriv() throws Xcept {
	    Expr nexpr = expr.expandDeriv();
	    return new UnitCast(nexpr, unit);
	}
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    Expr nexpr = expr.takeDomDeriv(t);
	    Unit nunit = nexpr.unit().div(t.unit());
	    return new UnitCast(nexpr, nunit);
	}

	// add to lists
	public void addDomains(Expr.List list) {
	    expr.addDomains(list);
	}
	public void addNamedExpr(Expr.List list) throws Xcept {
	    expr.addNamedExpr(list);
	}
}
