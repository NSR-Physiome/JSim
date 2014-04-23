/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// if-else expression

package JSim.expr;
import JSim.util.*;

public class IfExpr extends IExpr {
	Unit unit;	// set at unitCorrect()

	// constructor
	public IfExpr(Expr e1, Expr e2, Expr e3) throws Xcept {
	    super(IF, new Expr[] { e1, e2, e3 });
	    if (e1.dataType() != BOOLEAN) throw new Xcept(this,
		"BOOLEAN if argument required");
	    if (e2.dataType() != e3.dataType()) throw new Xcept(this,
		"matching if argument dataTypes required");
	    unit = null;
	}

	public String toString(Lang lang, Context ctxt) {
	    Expr a = args[0];
	    String as = ((ctxt == null) ? a.toString() : a.toString(ctxt));
	    Expr b = args[1];
	    String bs = ((ctxt == null) ? b.toString() : b.toString(ctxt));
	    bs = lang.castString(b, bs);
	    Expr c = args[2];
	    String cs = ((ctxt == null) ? c.toString() : c.toString(ctxt));
	    cs = lang.castString(c, cs);
	    return lang.ifStr(as, bs, cs);
	}

	// query
	public int dataType() { return args[1].dataType(); }
	public Unit unit() { return unit; }

	// evaluation
	public double realVal(Context ctxt) throws Xcept {	
	    if (dataType() != REAL) throw new Xcept(this,
		"Statement does not have realVal()");
	    boolean a = args[0].boolVal(ctxt);
	    double b = args[1].realVal(ctxt);
	    double c = args[2].realVal(ctxt);
	    return a?b:c; 
	}
	public boolean boolVal(Context ctxt) throws Xcept {	
	    if (dataType() != BOOLEAN) throw new Xcept(this,
		"Statement does not have boolVal()");
	    boolean a = args[0].boolVal(ctxt);
	    boolean b = args[1].boolVal(ctxt);
	    boolean c = args[2].boolVal(ctxt);
	    return a?b:c; 
	}

	// return unit corrected version of this
	public Expr unitCorrect() throws Xcept {
	    Expr a = args[0].unitCorrect(); 
	    Unit au = a.unit();
	    Expr b = args[1].unitCorrect(); 
	    Unit bu = b.unit();
	    Expr c = args[2].unitCorrect(); 
	    Unit cu = c.unit();
	    c = c.multUnit(bu, cu);
	    checkDimless(a,au); 
	    sameDim(b,c,bu,cu);
	    Unit unit = (bu==null) ? cu : bu;

	    // return corrected
	    IfExpr ret = new IfExpr(a, b, c);
	    ret.unit = unit;
	    return ret; 
	}


	// derivative
	public Expr takeDomDeriv(NamedExpr t) throws Xcept {
	    if (! t.isDomain()) throw new Xcept(t,
		"takeDomDeriv() requires domain");
	    Expr a = args[0];
	    Expr bt = args[1].takeDomDeriv(t);
	    Expr ct = args[2].takeDomDeriv(t);
	    return new IfExpr(a, bt, ct);
	}

	// simplify 
	public Expr simplify() throws Xcept {
	    if (isConst()) return cons();
	    Expr a = args[0].simplify();
	    Expr b = args[1].simplify();
	    Expr c = args[2].simplify();
	    if (a.isConst()) 
		return a.constBoolVal() ? b : c;
	    return new IfExpr(a, b, c);
	}
}
