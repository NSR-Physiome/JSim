/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MathML external csymbol definition

package JSim.mathml;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public abstract class MLCSymbol implements Named {
	protected String name;
	
	// constructor
	public MLCSymbol(String n) {
	    name = n;
	}
	
	// query
	public String name() { return name; }
	public String diagInfo() { return "CSymbol " + name; }

	// make expr
	abstract public Expr makeExpr(Expr.List args) throws Xcept;
	
	// MLCSymbol.NList
	public static class NList extends NamedList {
	    public NList() { super(); }
	    public MLCSymbol csym(int i) {
	        return (MLCSymbol) get(i);
	    }
	    public MLCSymbol csym(String n) {
	    	return (MLCSymbol) getByName(n);
  	    }
	}
}
