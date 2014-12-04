/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// logical expressions (and, or)

package JSim.expr;
import JSim.util.*;

public class LogicalExpr extends IExpr {

	// constructors
	public LogicalExpr(int opp, Expr e1, Expr e2) throws Xcept {
	    super(opp, new Expr[] { e1,e2 } );
	    if (e1.dataType() != BOOLEAN || e2.dataType() != BOOLEAN) 
	        throw new Xcept(this, "BOOLEAN arguments required");
	}

	// query methods
	public Unit unit() { return null; }
	public int dataType() { return BOOLEAN; }
	
	// evaluation
	public boolean boolVal(Context ctxt) throws Xcept {	
	    boolean a=args[0].boolVal(ctxt);
	    boolean b=args[1].boolVal(ctxt);

	    switch(op) {
	    case AND: return a && b; 
	    case OR: return a || b; 
	    }
	    throw new Xcept("LogicalExpr.boolVal() not supported for " + jsname(op));
	}

	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    Expr b = args[1].unitCorrect(); 
	    return new LogicalExpr(op, a, b);
	}


	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    throw new Xcept(this, 
	    	jsname(op) + " derivative not supported");
	}	    

	// simplify
	public Expr simplify() throws Xcept {
	    if (isConst()) return cons();
	    Expr a = args[0].simplify();
	    Expr b = args[1].simplify();
	    switch (op) {
	    case OR:
		if (a.isConst())
		    return a.constBoolVal() ? truex : b;
		if (b.isConst())
		    return b.constBoolVal() ? truex : a;
		if (a.sameAs(b)) return a;
		break;
	    case AND:
		if (a.isConst())
		    return a.constBoolVal() ? b : falsex;
		if (b.isConst())
		    return b.constBoolVal() ? a : falsex;
		if (a.sameAs(b)) return a;
	    }
	    return new LogicalExpr(op, a, b);
	}
}
