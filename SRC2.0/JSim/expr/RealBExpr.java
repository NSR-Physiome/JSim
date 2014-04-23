/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Binary built-in expression

package JSim.expr;
import JSim.util.*;

public class RealBExpr extends IExpr {
	Unit unit; 	// set at unitCorrect()

	// constructors
	public RealBExpr(int opp, Expr e1, Expr e2) throws Xcept {
	    super(opp, new Expr[] { e1, e2});
	    if (e1.dataType() != REAL || e2.dataType() != REAL) 
	        throw new Xcept(this, "REAL arguments required");
	    unit = null;
	}

	// simple query methods
	public int dataType() { return REAL; }
	public Unit unit() { return unit; }

	// need parentheses?
	public boolean needsParen(int argInx, int binop) {
	    int pop = precedence(op);
	    int pbinop = precedence(binop);
	    if (pop > pbinop) return false;
	    if (pop < pbinop) return true;
	    if (argInx == 0) return false;
	    // reminder: ADD,MULT not computationally associative!
	    return true;
	}

	// calc precedence for operator
	public int precedence(int op) {
	    switch (op) {
	    case ADD:
	    case SUB:
	    	return 1;
	    case MULT:
	    case DIV:
	    case REM:
	    	return 2;
	    case POW:
	    	return 3;
	    default:
	    	return 4;
	    }
	}
	        	
	// evaluation
	public double realVal(Context ctxt) throws Xcept {	
	    double a=args[0].realVal(ctxt);
	    double b=args[1].realVal(ctxt);
	    switch(op) {
	    case ADD: return a+b; 
	    case SUB: return a-b; 
	    case MULT: return a*b; 
	    case DIV: return a/b; 
	    case POW: return Math.pow(a,b); 
	    case REM: return UtilMath.rem(a,b); 
	    case ATAN: return Math.atan2(b,a); 
	    case BESSELJN: return UtilMath.besselJn(a,b);
	    case BESSELKN: return UtilMath.besselKn(a,b);
	    case BESSELYN: return UtilMath.besselYn(a,b);
	    }
	    throw new Xcept("RealBExpr.realVal() not supported for " + jsname(op));
	}

	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    Unit au = a.unit();
	    Expr b = args[1].unitCorrect(); 
	    Unit bu = b.unit();
 
	    // figure return unit
	    Unit unit = Unit.bogus();
	    switch(op) {
//   
	    case BESSELJN:
	    case BESSELKN:
	    case BESSELYN:
	        checkDimless(a,au);
	        checkDimless(b,bu);
	        unit = Unit.scalar();
	        a = a.multUnit(unit, au);
	        b = b.multUnit(unit, bu);
	        break;
//
	    case ADD: 
	    case SUB: 	
	    case ATAN:
	    case REM:
		if (au == null && bu == null) 
		    unit = null;
		else {
		    if (au == null) 
			au = Unit.compatible(bu, Unit.scalar()) ? 
			    Unit.scalar() : bu;
		    if (bu == null) 
			bu = Unit.compatible(au, Unit.scalar()) ? 
			    Unit.scalar() : au;
		    sameDim(a,b,au,bu);
		    b = b.multUnit(au, bu); 
		    unit = au;
		}
		break;
	    case MULT: 
	    case DIV: 
	    case DERIV:
		if (au == null && bu == null)
		    unit = null;
		else {
		    if (au == null) au = Unit.scalar();
		    if (bu == null) bu = Unit.scalar();
		    unit = au.binop(bu, op);
		}
		break;
	    case POW: 
		if (!Unit.compatible(bu, Unit.scalar()))
		    throw new Xcept(this, b,
			"Exponents must be dimensionless");
		if (bu != null && !Util.nearlyZero(bu.f-1)) {
		    b = b.mult(Expr.cons(bu.f));
		    bu = Unit.scalar();
		}
		if (Unit.compatible(au, Unit.scalar())) {
		    if (au != null && !Util.nearlyZero(au.f-1)) {
		    	a = a.mult(Expr.cons(au.f));
		    	au = Unit.scalar();
		    }
		    unit = au;
		} else {	
		    if (!b.isConstIgnoreUnits()) 
		    throw new Xcept(this,
			"Cannot unit-correct variable exponents");
		    double bv = b.constRealVal();
		    unit = au.power(bv);
		}
		break;
	    }
	    if (unit == Unit.bogus()) 
		throw new Xcept(this, "Unsupported op in RealBExpr.unit()"); 		

	    // return corrected
	    RealBExpr ret = new RealBExpr(op, a, b);
	    ret.unit = unit;
	    return ret; 
	}


	// derivative
	//   NOTE: MULT/DIV/... have zero checks here to ensure 
	//   expanded derivs pass unit balance later
	//   since constants are not currently assigned units   
	//   ??? simplify() probably should not be called here
	//      (no simplify() calls before unit correction)
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    if (! t.isDomain()) throw new Xcept(t,
		"takeDomDeriv() requires domain");
	    Expr a = args[0];
	    Expr b = args[1];
	    Expr at = a.takeDomDeriv(t);
	    Expr bt = b.takeDomDeriv(t);
	    Expr u1, u2, u3, u4;
	    switch (op) {
	    case ADD:
		return at.add(bt);
	    case SUB:
		return at.sub(bt);
	    case MULT:
		u1 = a.mult(bt);
		if (isZero(bt)) u1 = zero;
		u2 = at.mult(b);
		if (isZero(at)) u2 = zero;
		return u1.add(u2);
	    case DIV:  // (a/b)' = (a'b - b'a)/b^2
		u1 = at.mult(b);
		if (isZero(at)) u1 = zero;
		u2 = bt.mult(a);
		if (isZero(bt)) u2 = zero;
		u3 = b.pow(2);
		return u1.sub(u2).div(u3);
	    case POW: // (a^b)' = (a^(b-1))(ba' + ab'ln(a))
		u1 = a.pow(b.sub(one));
		u2 = b.mult(at);
		if (isZero(at)) u2 = zero;
		u3 = a.mult(bt).mult(new RealUExpr(IExpr.LN, a));
		if (isZero(bt)) u3 = zero;
		return u1.mult(u2.add(u3));
	    case ATAN: // atan(y/x)' = (xy' - yx')/(x^2 + y^2)
		u1 = b.mult(at);
		if (isZero(at)) u1 = zero;
		u2 = a.mult(bt);
		if (isZero(bt)) u2 = zero;
		u3 = a.pow(2).add(b.pow(2));
		return u1.sub(u2).div(u3);
	    case BESSELJN:  // besseljn(a,b)' = b'*(besseljn(a,b)*(a/b)-besseljn(a+1,a) ; 
	        u1 =  new RealBExpr (BESSELJN, a, b);
	        u2 = one;
	        u3 =  new RealBExpr (BESSELJN, u2.add(a), b);
	        u4 =  new RealUExpr (ROUND, a);
	        return bt.mult(u1.mult(u4.div(b)).sub(u3));
	    case BESSELKN:  // besselkn(a,b)' = b'*(besselkn(a,b)*(a/b)-besselkn(a+1,a) ; 
	        u1 =  new RealBExpr (BESSELKN, a, b);
	        u2 = one;
	        u3 =  new RealBExpr (BESSELKN, u2.add(a), b);
	        u4 =  new RealUExpr (ROUND, a);
	        return bt.mult(u1.mult(u4.div(b)).sub(u3));
	    case BESSELYN:  // besselyn(a,b)' = b'*(besselyn(a,b)*(a/b)-besselyn(a+1,a) ; 
	        u1 =  new RealBExpr (BESSELYN, a, b);
	        u2 = one;
	        u3 =  new RealBExpr (BESSELYN, u2.add(a), b);
	        u4 =  new RealUExpr (ROUND, a);
	        return bt.mult(u1.mult(u4.div(b)).sub(u3));
	    case REM: // rem(a,b)' = a' - floor(a/b)*b'
		u1 = new RealUExpr(FLOOR, a.div(b));
		return at.sub(u1.mult(bt));
	    }
	    throw new Xcept(this, 
	    	jsname(op) + " derivative not supported");
	}

	// is expr constant 0
	private boolean isZero(Expr expr) throws Xcept {
	    return expr.isConst() && (expr.constRealVal() == 0);
	}

	// expand derivatives
	public Expr expandDeriv() throws Xcept {
	    if (op != DERIV) return super.expandDeriv();
	    Expr a = args[0].expandDeriv();
	    Expr b = args[1].expandDeriv();
	    return a.takeDeriv(b);
	}

	// simplify
	public Expr simplify() throws Xcept {
	    if (isConst()) return cons();
	    Expr a = args[0].simplify();
	    Expr b = args[1].simplify();
	    switch(op) {
	    case ADD:
		if (b.isConst() && (b.constRealVal() == 0)) 
		    return a;
		if (a.isConst() && (a.constRealVal() == 0)) 
		    return b;
		break;
	    case SUB:
		if (b.isConst() && (b.constRealVal() == 0)) 
		    return a;
		if (a.isConst() && (a.constRealVal() == 0)) 
		    return b.mult(negone).simplify();
		if (a.sameAs(b)) return zero;
		break;
	    case MULT:
	    case DIV:
		return Simplifier.simplify(this);
/*
	    case DIV:
		if (isOne(b)) 
		    return a;
		if (a.isConst() && (a.constRealVal() == 0)) 
		    return zero;
		if (a == b) return one;
		break;
	    case MULT:
		if (isOne(b)) 
		    return a;
		if (isOne(a)) 
		    return b;
		if (b.isConst() && (b.constRealVal() == 0)) 
		    return zero;
		if (a.isConst() && (a.constRealVal() == 0)) 
		    return zero;
		break;
*/
	    case POW:
		if (isOne(b)) 
		    return a;
		if (b.isConst() && (b.constRealVal() == 0)) 
		    return one;
		break;
	    }
	    return new RealBExpr(op, a, b);
	}
	    
	// check if dimensionless one
	private boolean isOne(Expr e) throws Xcept {
	    if (! e.isConst()) return false;
	    if (e.constRealVal() != 1) return false;
	    Unit u = e.unit();
	    if (u == null) return true;
	    if (Unit.same(u, Unit.scalar())) return true;
	    return false;
	}

	// expand sum 
	public Expr.List expandSum() throws Xcept {
	    Expr.List list = new Expr.List(2);
	    Expr a = args[0];
	    Expr b = args[1];
	    Expr.List alist, blist;
	    switch (op) {
	    case ADD:
		list.add(a);
		list.add(b);
		break;
	    case SUB:
		list.add(a);
		list.add(b.mult(negone));
		break;
	    case MULT:
		alist = a.expandSum();
		blist = b.expandSum();
		for (int i=0; i<alist.size(); i++) {
		    Expr a1 = alist.expr(i);
		    for (int j=0; j<blist.size(); j++) {
			Expr b1 = blist.expr(j);
			list.add(a1.mult(b1));
		    }
		}
		break;
	    case DIV:
		alist = a.expandSum();
		for (int i=0; i<alist.size(); i++) {
		    Expr a1 = alist.expr(i);
		    list.add(a1.div(b));
		}
		break;
	    default:
		list.add(this);
		break;
	    }

	    return list;
	}

	// find linear factor
	public Expr linearFactor(NamedQueryExpr c, boolean keep) throws Xcept {
	    Expr a = args[0];
	    Expr b = args[1];
	    Expr a_c, b_c, a__c, b__c;
	    switch (op) {
	    case ADD:
	    	a_c = a.linearFactor(c, keep);
	    	b_c = b.linearFactor(c, keep);
		return a_c.add(b_c);				
	    case SUB:
	    	a_c = a.linearFactor(c, keep);
	    	b_c = b.linearFactor(c, keep);
		return a_c.sub(b_c);				
	    case MULT:
		a_c = a.linearFactor(c, true);
		b_c = b.linearFactor(c, true);
		a__c = a.linearFactor(c, false);
		b__c = b.linearFactor(c, false);
		if (keep) 
		    return a_c.mult(b__c).add(a__c.mult(b_c));
		Expr ret = a__c.mult(b__c);
		if (!isConst())  
		    ret = ret.add(a_c.mult(b_c).mult(c).mult(c));
		return ret;
	    case DIV:
	    	a_c = a.linearFactor(c, keep);
		return a_c.div(b);
	    default:
		return super.linearFactor(c, keep);
	    }
	}
}
