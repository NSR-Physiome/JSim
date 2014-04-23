/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// consumption of chemical in region

package JSim.bcl.mfax;

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class Consumption extends Production {

	// constructor
	public Consumption(Comp p, String n, Expr.List e) throws Xcept {
	    super(p, n, e);
	}

	// delta concentration
	public Expr concDelta(Chem ch) throws Xcept {
	    if (chem != ch) return Expr.zero;
	    Expr ztest = new CompareExpr(IExpr.GT,
		region.conc(chem), Expr.zero);
	    return new IfExpr(ztest, 
		Expr.negone.mult(flux.div(region.vol)), Expr.zero);
	}

}
