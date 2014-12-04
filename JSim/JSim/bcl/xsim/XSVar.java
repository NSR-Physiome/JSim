/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// real input variable

package JSim.bcl.xsim;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class XSVar {

	// common logic for all variables
	public static void common(Var v) throws Xcept {
	    v.addProp("loc", Expr.REAL);
	    Domain x = ((XSSys) v.getSys()).xdomain(v);
	    if (x == null) return;
	    v.addProp("locincr", Expr.REAL);
	    v.addProp("dim", Expr.REAL);
	}

	// RealInput 
	public static class RealInput extends RealNVar {

	    // constructor
	    public RealInput(Comp p, String n, Expr.List e) 
	    throws Xcept {
	    	super(p, n, e);
		common(this);
	    }
	}

	// IntInput 
	public static class IntInput extends IntNVar {

	    // constructor
	    public IntInput(Comp p, String n, Expr.List e) 
	    throws Xcept {
	    	super(p, n, e);
		common(this);
	    }
	}

	// ChoiceInput 
	public static class ChoiceInput extends ChoiceNVar {

	    // constructor
	    public ChoiceInput(Comp p, String n, Expr.List e) 
	    throws Xcept {
	    	super(p, n, e);
		common(this);
	    }
	}

	// RealOutput 
	public static class RealOutput extends RealNVar {

	    // constructor
	    public RealOutput(Comp p, String n, Expr.List e) 
	    throws Xcept {
	    	super(p, n, e);
		common(this);
	    }
	}
}

