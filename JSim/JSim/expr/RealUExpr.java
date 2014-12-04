/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// Real-valued Unary expression

package JSim.expr;
import JSim.util.*;

public class RealUExpr extends IExpr {
	Unit unit;	// set at unitCorrect

	// constructor
	public RealUExpr(int opp, Expr e) throws Xcept {
	    super(opp, new Expr[] { e });
	    if (e.dataType() != REAL) throw new Xcept(this,
		"REAL argument required");
	    unit = null;
	}

	// query
	public int dataType() { return REAL; }
	public Unit unit() { return unit; }
	public boolean needsParen(int argInx, int binop) { return false; }

	// evaluation
	public double realVal(Context ctxt) throws Xcept {	
	    double a=args[0].realVal(ctxt);

	    switch(op) {
	    case ABS: return Math.abs(a); 
	    case CEIL: return Math.ceil(a); 
	    case FLOOR: return Math.floor(a); 
	    case ROUND: return Math.round(a); 
	    case LN: return Math.log(a); 
	    case LOG: return Math.log(a)/Math.log(10.0); 
	    case SIN: return Math.sin(a); 
	    case COS: return Math.cos(a); 
	    case TAN: return Math.tan(a); 
	    case EXP: return Math.exp(a); 
	    case SQRT: return Math.sqrt(a); 
	    case ASIN: return Math.asin(a); 
	    case ACOS: return Math.acos(a); 
	    case SINH: return UtilMath.sinh(a); 
	    case COSH: return UtilMath.cosh(a); 
	    case TANH: return UtilMath.tanh(a); 
	    case ASINH: return UtilMath.asinh(a); 
	    case ACOSH: return UtilMath.acosh(a); 
	    case ATANH: return UtilMath.atanh(a); 
	    case BESSELI0: return cern.jet.math.Bessel.i0(a);
	    case BESSELI1: return cern.jet.math.Bessel.i1(a);
	    case ERF: return cern.jet.stat.Probability.errorFunction(a);
	    case ERFC: return cern.jet.stat.Probability.errorFunctionComplemented(a);
	    }
	    throw new Xcept("RealUExpr.realVal() not supported for " + jsname(op));
	}

	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    Unit au = a.unit();

	    // figure return unit
	    Unit unit = Unit.bogus();
	    switch(op) {
	    case ABS: 
		unit = au; // OK if null
		break;
	    case LN: 
	    case LOG: 
	    case SIN: 
	    case COS: 
	    case TAN: 
	    case EXP: 
	    case ASIN: 
	    case ACOS: 
	    case SINH: 
	    case COSH: 
	    case TANH: 
	    case ASINH: 
	    case ACOSH: 
	    case ATANH: 
	    case FLOOR: // otherwise floor(.5 kg) != floor(500 g)
	    case CEIL: 
	    case ROUND: 
	    case BESSELI0:
	    case BESSELI1:
	    case ERF:
	    case ERFC:
	    	checkDimless(a, au);
		unit = Unit.scalar();  // was au
		a = a.multUnit(unit, au); 
		break;   
	    case SQRT:
		if (au == null)
		    unit = null;
		else
  		    unit = au.power(0.5);
		break; 
	    }
	    if (unit == Unit.bogus()) 
		throw new Xcept(this, "Unsupported op in UExpr.unit()"); 		

	    // return corrected
	    RealUExpr ret = new RealUExpr(op, a);
	    ret.unit = unit;
	    return ret; 
	}

	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    if (! t.isDomain()) throw new Xcept(t,
		"takeDomDeriv() requires domain");
	    Expr a = args[0];
	    Expr at = a.takeDomDeriv(t);
	    Expr u1, u2, u3, u4, u5, u6;
	    switch (op) {
	    case SQRT:	// sqrt(a)' = a'/2*sqrt(a)
		u1 = cons(2);
		u2 = new RealUExpr(SQRT, a);
		return at.div(u1.mult(u2));
	    case EXP: // exp(a)' = a' * exp(a)
		return at.mult(new RealUExpr(EXP, a));
	    case LN:  // ln(a)' = a'/a
		return at.div(a);		
	    case LOG: // log(a)' = ln(10)*a'/a
		u1 = cons(10);
		u2 = new RealUExpr(LN, u1);
		return u2.mult(at).div(a);
	    case SIN:  // sin(a)' = a' * cos(a)
		return at.mult(new RealUExpr(COS, a));
	    case COS:  // cos(a)' = -a' * sin(a)
		u1 = at.mult(new RealUExpr(SIN, a));
		return u1.mult(negone);
	    case TAN: // tan(a)' = a' / cos(a)^2 
		u1 = new RealUExpr(COS, a);
		return at.div(u1.pow(2));
	    case IExpr.ASIN: // asin(a)' = a'/sqrt(1-a^2)
		u1 = one;
		u2 = new RealUExpr(SQRT, u1.sub(a.pow(2))); 
		return at.div(u2);
	    case IExpr.ACOS: // acos(a)' = -a'/sqrt(1-a^2)
		u1 = one;
		u2 = new RealUExpr(SQRT, u1.sub(a.pow(2))); 
		u3 = negone;
		return at.div(u2).mult(u3);
	    case IExpr.SINH: // sinh(a)' = a' * cosh(a)
		u1 = new RealUExpr(COSH, a);
		return at.mult(u1);
	    case IExpr.COSH: // cosh(a)' = a' * sinh(a)
		u1 = new RealUExpr(SINH, a);
		return at.mult(u1);
	    case IExpr.TANH: // tanh(a)' = a' * (1-tanh(a)^2)
		u1 = new RealUExpr(TANH, a);
		u2 = one.sub(u1.pow(2));
		return at.mult(u2);
	    case IExpr.ASINH: // asinh(a)' = a'/sqrt(1+a^2)
		u1 = new RealUExpr(SQRT, one.add(a.pow(2))); 
		return at.div(u1);
	    case IExpr.ACOSH: // acosh(a)' = a'/sqrt(a^2-1)
		u1 = new RealUExpr(SQRT, a.pow(2).sub(one)); 
		return at.div(u1);
	    case IExpr.ATANH: // atanh(a)' = a'/(1-a^2)
		u1 = one.sub(a.pow(2)); 
		return at.div(u1);
	    case IExpr.BESSELI0: // besseli0(a)'=a'*besseli1(a);
	        u1 = new RealUExpr(BESSELI1, a);
	        return at.mult(u1);
	    case IExpr.BESSELI1: // besseli1(a)'=a'*(besseli0(a)-besseli1(a)/a );
	        u1 = new RealUExpr(BESSELI0, a);
	        u2 = new RealUExpr(BESSELI1, a);
	        return at.mult(u1.sub(u2.div(a)));
	    case IExpr.ERF: // erf(a)' = a'*(2/sqrt(PI))*exp( -a*a) ;
	        u1 = negone;
	        u2 = one;
	        u3 = pi;
	        u4 = u2.div(u3);
	        u5 = new RealUExpr(SQRT, u4 );
	        u6 = new RealUExpr(EXP, u1.mult(a.pow(2)) );
	        return at.mult(u6.mult(u5.add(u5)));
	    case IExpr.ERFC: // erfc(a)' = -a'*(2/sqrt(PI))*exp( -a*a) ;
	        u1 = negone;
	        u2 = one;
	        u3 = pi;
	        u4 = u2.div(u3);
	        u5 = new RealUExpr(SQRT, u4 );
	        u6 = new RealUExpr(EXP, u1.mult(a.pow(2)) );
	        return at.mult(u6.mult(u5.add(u5))).mult(u1);
	    case IExpr.ABS: // abs(a)' = if (a>=0) a' else -a';
		u1 = new CompareExpr(GE, a, zero);
		u2 = negone;
		return new IfExpr(u1, at, at.mult(u2));
	    case IExpr.CEIL:
	    case IExpr.FLOOR:
	    case IExpr.ROUND:
		return zero;
	    }
	
	    throw new Xcept(this,
		jsname(op) + " derivative not implemented");
	}

}
