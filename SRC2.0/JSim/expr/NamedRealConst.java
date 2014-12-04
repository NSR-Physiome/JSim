/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Named real constant

package JSim.expr;
import JSim.util.*;

public class NamedRealConst extends NamedExpr {
	private String name;
	private double value;

	    // constructor
	    public NamedRealConst(String n, double d) { 
	    	name = n; 
		value = d;
	    }
	    
	    // query
	    public String name() { return name; }
	    public String diagInfo() { return "Xpr " + name; }
	    public String toString() { return name; }
	    public String toString(Context ctxt) { 
	    	return ctxt.newName(this); 
	    }
	    public Expr takeDomDeriv(NamedExpr e) {
	        return this;
	    }
	    public Expr expandDeriv() { return this; }
	    public Expr unitCorrect() { return this; }
	    public Unit unit() { return null; }
	    public void addDomains(Expr.List list) {  }
	    public boolean sameAs(Expr e) { 
	        if (! (e instanceof NamedRealConst)) return false;
		NamedRealConst ne = (NamedRealConst) e;
		return ne.name.equals(name) && ne.value == value;
	    }
	    public int dataType() { return REAL; }
	    public Expr simplify() { return this; }
}
