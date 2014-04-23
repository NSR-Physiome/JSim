/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MathML <apply> operator table

package JSim.mathml;

import JSim.util.*;
import JSim.expr.*;

public class MLApply extends NamedList {


	// constructor
	public MLApply() throws Xcept {
	    super(71);

	    new ConstOp(this, "pi", Math.PI);
	    new IExprOp(this, "eq", IExpr.EQ);
	    new IExprOp(this, "neq", IExpr.NE);
	    new IExprOp(this, "gt", IExpr.GT);
	    new IExprOp(this, "lt", IExpr.LT);
	    new IExprOp(this, "geq", IExpr.GE);
	    new IExprOp(this, "leq", IExpr.LE);
	    new NestedBOp(this, "plus", IExpr.ADD);
	    new NestedBOp(this, "times", IExpr.MULT);
	    new IExprOp(this, "divide", IExpr.DIV);
	    new IExprOp(this, "power", IExpr.POW);
	    new IExprOp(this, "rem", IExpr.REM);
	    new IExprOp(this, "abs", IExpr.ABS);
	    new IExprOp(this, "exp", IExpr.EXP);
	    new IExprOp(this, "ln", IExpr.LN);
//	    new IExprOp(this, "log", IExpr.LOG);
	    new IExprOp(this, "floor", IExpr.FLOOR);
	    new IExprOp(this, "ceiling", IExpr.CEIL);
	    new NestedBOp(this, "and", IExpr.AND);
	    new NestedBOp(this, "or", IExpr.OR);
	    new IExprOp(this, "not", IExpr.NOT);
	    new IExprOp(this, "sin", IExpr.SIN);
	    new IExprOp(this, "cos", IExpr.COS);
	    new IExprOp(this, "tan", IExpr.TAN);
	    new IExprOp(this, "arcsin", IExpr.ASIN);
	    new IExprOp(this, "arccos", IExpr.ACOS);

	    new IExprOp(this, "sinh", IExpr.SINH);
	    new IExprOp(this, "cosh", IExpr.COSH);
	    new IExprOp(this, "tanh", IExpr.TANH);
	    new IExprOp(this, "arcsinh", IExpr.ASINH);
	    new IExprOp(this, "arccosh", IExpr.ACOSH);
	    new IExprOp(this, "arctanh", IExpr.ATANH);
	    new IExprOp(this, "besseli0", IExpr.BESSELI0);
	    new IExprOp(this, "besseli1", IExpr.BESSELI1);
	    new IExprOp(this, "besseljn", IExpr.BESSELJN);
	    new IExprOp(this, "besselkn", IExpr.BESSELKN);
	    new IExprOp(this, "besselyn", IExpr.BESSELYN);
	    new IExprOp(this, "erf", IExpr.ERF);
	    new IExprOp(this, "erfc", IExpr.ERFC);

	    // minus may be unary or binary
	    new IExprOp(this, "minus", IExpr.SUB) {
		public Expr makeExpr(Expr.List args) throws Xcept {
		    if (args.size() == 1)
			return Expr.negone.mult(args.expr(0));
		    return super.makeExpr(args);
		}
	    };

	    // root op may have 1 or 2 args
	    new IExprOp(this, "root", IExpr.SQRT) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() == 2) {
                    Expr pow = Expr.one.div(args.expr(0));
                    Expr base = args.expr(1);
                    return base.pow(pow);
                }
                return super.makeExpr(args);
            }
	    };

	    // arctan op has 1 arg, but JSim needs two
	    new IExprOp(this, "arctan", IExpr.ATAN) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arctan requires 1 argument in MathML");
                return new RealBExpr(IExpr.ATAN, Expr.one, args.expr(0)) ;
            }
	    };

	    // secant(x) = 1/cos(x)
	    new IExprOp(this, "sec", IExpr.COS) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("secant requires 1 argument in MathML");
                RealUExpr cos = new RealUExpr(IExpr.COS, args.expr(0));
                return Expr.one.div(cos);
            }
	    };

	    // cosecant(x) = 1/sin(x)
	    new IExprOp(this, "csc", IExpr.SIN) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("cosecant requires 1 argument in MathML");
                RealUExpr sin = new RealUExpr(IExpr.SIN, args.expr(0));
                return Expr.one.div(sin);
            }
	    };

	    // cotangent(x) = 1/tan(x)
	    new IExprOp(this, "cot", IExpr.TAN) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("cotangent requires 1 argument in MathML");
                RealUExpr tan = new RealUExpr(IExpr.TAN, args.expr(0));
                return Expr.one.div(tan);
            }
	    };

	    // arcsec(x) = acos(1/x)
	    new IExprOp(this, "arcsec", IExpr.ACOS) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arcsec requires 1 argument in MathML");
                return new RealUExpr(IExpr.ACOS, Expr.one.div(args.expr(0)));
            }
	    };

	    // arccosec(x) = asin(1/x)
	    new IExprOp(this, "arccsc", IExpr.ASIN) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arccsc requires 1 argument in MathML");
                return new RealUExpr(IExpr.ASIN, Expr.one.div(args.expr(0)));
            }
	    };

	    // arccotan(x) = atan(1/x)
	    new IExprOp(this, "arccot", IExpr.ATAN) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arccot requires 1 argument in MathML");
                return new RealBExpr(IExpr.ATAN, Expr.one, Expr.one.div(args.expr(0)));
            }
	    };

	    // arcsech(x) = acosh(1/x)
	    new IExprOp(this, "arcsech", IExpr.ACOSH) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arcsech requires 1 argument in MathML");
                return new RealUExpr(IExpr.ACOSH, Expr.one.div(args.expr(0)));
            }
	    };

	    // arccosech(x) = asin(1/x)
	    new IExprOp(this, "arccsch", IExpr.ASINH) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arccsch requires 1 argument in MathML");
                return new RealUExpr(IExpr.ASINH, Expr.one.div(args.expr(0)));
            }
	    };

	    // arccotanh(x) = atanh(1/x)
	    new IExprOp(this, "arccoth", IExpr.ATANH) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() != 1) throw new Xcept
                    ("arccoth requires 1 argument in MathML");
                return new RealUExpr(IExpr.ATANH, Expr.one.div(args.expr(0)));
            }
	    };

	    // 1 op -> log(a),  1 op = log(b)/log(a) (1st op is logbase)
	    new IExprOp(this, "log", IExpr.LOG) {
	    	public Expr makeExpr(Expr.List args) throws Xcept {
                if (args.size() == 1) 
 		    return IExpr.create(opcode, args);
 ///OLD             super.makeExpr(args);
                if (args.size() != 2) throw new Xcept
                    ("log requires 1 or 2 args");
                Expr base = args.expr(0);
                Expr a = args.expr(1);
                if (base.isConst() && base.realVal(null) == 10)
                    return new RealUExpr(IExpr.LOG, a);
                Expr la = new RealUExpr(IExpr.LN, a);
                Expr lbase = new RealUExpr(IExpr.LN, base);
                return la.div(lbase);
            }
	    };

	    // derivative uses reversed arguments
	    new IExprOp(this, "diff", IExpr.DERIV) {
		public Expr makeExpr(Expr.List args) throws Xcept {
		    int ct = args.size();
		    Expr.List nargs = new Expr.List(ct);
		    for (int i=0; i<ct; i++) 
			nargs.add(args.expr(ct-1-i));
//		    MLVar v0 = (nargs.expr(0) instanceof MLVar) ?
//		    	((MLVar) nargs.expr(0)) : null;
//		    MLVar v1 = (nargs.expr(1) instanceof MLVar) ?
//		    	((MLVar) nargs.expr(1)) : null;
//		    if (v0 != null) v0.hasDeriv = true;
//		    if (v1 != null) {
//		        v1.isDomain = true;
//			if (v0 != null) v0.domains.addUniq(v1);
//		    }
		    return super.makeExpr(nargs);
		}
	    };
	}

	// table query
	public Op op(String n) { return (Op) getByName(n); }

	// basic apply operator class
	public static class Op implements Named {
	    public String name;
	    public Op(MLApply apply, String n) { 
		name = n; 
		apply.add(this);
	    }
	    public String name() { return name; }
	    public String diagInfo() {
		return "<apply> operator <" + name + ">";
	    }
	    public Expr makeExpr(Expr.List args) throws Xcept {
	    	throw new Xcept(this, "Not yet supported");
	    }
	    public void checkNArgs(int ct, Expr.List args, boolean exact) 
	    throws Xcept {
	    	boolean pass = exact ? 
		    (ct == args.size()) : (ct <= args.size());
	    	if (pass) return;
	    	throw new Xcept(this,
		"Operator '" + name + "' requires " + 
		( exact ? "exactly " : "at least ") +
		ct + " args, " + args.size() + " found.");
	    }
	}

	// constant operator
	public static class ConstOp extends Op {
	    Expr constExpr;
	    public ConstOp(MLApply apply, String n, double v) throws Xcept {
		super(apply, n);
		constExpr = Expr.cons(v);
	    }
	    public Expr makeExpr(Expr.List args) throws Xcept {
		checkNArgs(0, args, true);
		return constExpr;
	    }
	}

	// IExpr-based operator
	public static class IExprOp extends Op {
	    public int opcode;
	    public int opct;
	    public IExprOp(MLApply apply, String n, int c) throws Xcept {
		super(apply, n);
		opcode = c;
		opct = IExpr.opct(opcode);
	    }
	    public Expr makeExpr(Expr.List args) throws Xcept {
		checkNArgs(opct, args, true);
		return IExpr.create(opcode, args);
	    }
	}

	// Dimensionless single arg IExpr
	//    not actually needed: Nickerson 21 Jul 05
	public static class DimlessOp extends IExprOp {
	    public DimlessOp(MLApply apply, String n, int c) throws Xcept {
		super(apply, n, c);
		if (opct != 1) throw new Xcept(
		    "DimlessOp requires opct=1: " + n);
	    }
	    public Expr makeExpr(Expr.List args) throws Xcept {
		checkNArgs(opct, args, true);
		Unit uarg = null;
//		try {
		    Expr carg0 = args.expr(0).unitCorrect();
		    uarg = carg0.unit();
		    if (! Unit.compatible(uarg, Unit.scalar())) {
		        args = new Expr.List(1);
		    	args.add(carg0.div(new RealConst(1, uarg)));
		    }
//		} catch (Xcept e) {
//		    Util.verbose("Unable to undimensionalize " +
//		       args.expr(0));
//		}
		Expr ret = IExpr.create(opcode, args);
		if (uarg != null) ret = ret.mult(new RealConst(1, uarg));
		return ret;
	    }
	}

	// nested binary IExpr-based operator
	public class NestedBOp extends IExprOp {
	    public NestedBOp(MLApply apply, String n, int c) throws Xcept {
		super(apply, n, c);
	    }
	    public Expr makeExpr(Expr.List args) throws Xcept {
            if (args.size()==0) {
                if (opcode == IExpr.ADD) {
                    return Expr.zero;
                }
                else if (opcode == IExpr.MULT) {
                    return Expr.one;
                }
                else if (opcode == IExpr.AND) {
                    return Expr.truex;
                }
                else if (opcode==IExpr.OR) {
                    return Expr.falsex;
                }
                else {
                    throw new Xcept("Unable to create operator " + name + "with zero arguments.");
                }
            }
            Expr expr = args.expr(0);
	    	for (int i=1; i<args.size(); i++) {
                Expr.List bargs = new Expr.List(2);
                bargs.add(expr);
                bargs.add(args.expr(i));
                expr = IExpr.create(opcode, bargs);
	    	}
	    	return expr;
	    }
	}
}

