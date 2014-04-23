/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Context for constant expression values

package JSim.expr;
import JSim.util.*;

public class ConstContext extends Context {

	// static stuff
	static public ConstContext jsim;
	static { jsim = new ConstContext(JSLang.lang); }

	// constructor
	public ConstContext(Lang l) {
	     super(l);
	}

	public String newName(Named n) {
	    return n.name();
	}

	public String funcCall(Named n, Expr.List elist) {
	    return n.name() + elist.toString(this);
	}

	public String unitCast(UnitCast cast) {
	    return "(" + cast.expr().toString(this) + " unit " + 
	    	cast.unit().pubName() + ")";
	}
}
