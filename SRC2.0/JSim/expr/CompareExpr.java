/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// compare 2 exprs

package JSim.expr;
import JSim.util.*;

public class CompareExpr extends IExpr {

	// constructors
	public CompareExpr(int opp, Expr e1, Expr e2) throws Xcept {
	    super(opp, new Expr[] { e1,e2 } );
	    if (e1.dataType() != e2.dataType()) 
	        throw new Xcept(this, "matching arguments dataTypes required");
	}

	// query methods
	public int dataType() { return BOOLEAN; }
	public Unit unit() { return null; }

	// evaluation
	public boolean boolVal(Context ctxt) throws Xcept {	
	    switch (args[0].dataType()) {
	    case REAL: return boolValReal(ctxt);
	    case BOOLEAN: return boolValBool(ctxt);
	    }
	    throw new Xcept(this, "boolVal() dataType unsupported");
	}
 
	// evaluation w/ real args
	public boolean boolValReal(Context ctxt) throws Xcept {	
	    double a=args[0].realVal(ctxt);
	    double b=args[1].realVal(ctxt);

	    switch(op) {
	    case EQ: return a==b; 
	    case NE: return a!=b; 
	    case LT: return a<b; 
	    case LE: return a<=b; 
	    case GT: return a>b; 
	    case GE: return a>=b; 
	    }
	    throw new Xcept("CompareExpr.boolValReal() op not supported for " + jsname(op));
	}

	// evaluation w/ boolean args
	public boolean boolValBool(Context ctxt) throws Xcept {	
	    boolean a=args[0].boolVal(ctxt);
	    boolean b=args[1].boolVal(ctxt);

	    switch(op) {
	    case EQ: return a==b; 
	    case NE: return a!=b; 
	    }
	    throw new Xcept("CompareExpr.boolValBool() op not supported for " + jsname(op));
	}

	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    Unit au = a.unit();
	    Expr b = args[1].unitCorrect(); 
	    Unit bu = b.unit();
 
	    // check for incompatibilities
	    if (au == null && bu != null) 
	        au = Unit.compatible(bu, Unit.scalar()) ? 
		   Unit.scalar() : bu;
	    if (bu == null && au != null) 
		bu = Unit.compatible(au, Unit.scalar()) ? 
		    Unit.scalar() : au;  
	    b = b.multUnit(au, bu);
	    sameDim(a,b,au,bu);

	    // return corrected
	    CompareExpr ret = new CompareExpr(op, a, b);
	    return ret; 
	}


	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    Expr at = args[0].takeDomDeriv(t);
	    Expr bt = args[1].takeDomDeriv(t);
	    return new CompareExpr(op, at, bt);
	}	    

	// simplify
	public Expr simplify() throws Xcept {
	    Expr e = super.simplify();
	    if (! CompareExpr.class.isInstance(e)) return e;
	    CompareExpr ce = (CompareExpr) e;
	    Expr a = ce.args[0];
	    Expr b = ce.args[1];
	    if (a.sameAs(b)) {
		switch(ce.op) {
		case EQ:
		case LE:
		case GE:
		case APPROX:
		    return Expr.truex;
		}
		return Expr.falsex;
	    }

	    switch (ce.op) {
	    case LE: e = a.eq(b).or(
		new CompareExpr(IExpr.LT, a, b));
		return e.simplify();
	    case GE: e = a.eq(b).or(
		new CompareExpr(IExpr.GT, a, b));
		return e.simplify();
	    }

	    return e;
	}
}
