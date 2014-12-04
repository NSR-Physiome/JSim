/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// MathML writer

package JSim.mathml;

import java.io.*;
import java.util.*;
import org.w3c.dom.Document; 
import org.w3c.dom.Element; 
import org.w3c.dom.Node; 
import org.w3c.dom.Text; 

import JSim.util.*;
import JSim.expr.*;
import JSim.mml.*;

public class MLWriter {
	private Document doc;
	private Element base;
	private boolean addParagraphs;

	// constructor
	public MLWriter(Element e) {
	    base = e;
	    doc = base.getOwnerDocument();
	    initNames();
	    addParagraphs = false;
	}
	
	//// PUBLIC API

	// set paragraphs property
	public void setParagraphs(boolean b) {
	    addParagraphs = b;
	}

	// add MML model
	public void add(Model model) throws Xcept {
	    MathSys math = model.getFlatMath();
	    for (int i=0; i<math.voidFuncCalls.size(); i++) 
	    	add(math.voidFuncCalls.expr(i));	    
	    for (int i=0; i<math.eqn.size(); i++) 
	        add(math.eqn.eqn(i));
	}    

	// add 1 eqn
	public void add(Eqn eqn) throws Xcept {
	    SubDom sdom = eqn.sdom();
	    Expr expr = eqn.expr();
	    if (sdom.isEntire()) {
	    	add(expr);
		return;
	    }
	    
	    // use MathML <forall> for MML when clause
	    Expr.List doms = new Expr.List(2);
	    expr.addDomains(doms);
	    Expr xexpr = sdom.expr;
	    Element apply = doc.createElement("apply");
	    Element forall = doc.createElement("forall");
	    apply.appendChild(forall);
	    for (int i=0; i<doms.size(); i++) 
	    	addBVar(apply, doms.expr(i).toString(), 1);
	    Element cond = doc.createElement("condition");
	    add(cond, xexpr);
	    apply.appendChild(cond);
	    add(apply, expr);
	    newWrapper().appendChild(apply);
	}

	// add expr
	public void add(Expr expr) throws Xcept {
	    add(newWrapper(), expr);
	}
	    
	//// PRIVATE CALCs

	// add Element to base
	//   later add option to repress paragraph tags for non-HTML
	private Element newWrapper() {
	    Element math = doc.createElement("math");
	    math.setAttribute("xmlns", 
		"http://www.w3.org/1998/Math/MathML");
	    if (addParagraphs) {
	    	Element p = doc.createElement("p");
	    	base.appendChild(p);
	    	p.appendChild(math);
	    } else {
	    	base.appendChild(math);
  	    }
	    return math;
	}
	
	// add any expr
	private void add(Element elem, Expr expr) throws Xcept {
	    if (expr instanceof NamedExpr)
	    	addNamed(elem, (NamedExpr) expr);
	    else if (expr instanceof RealConst)
	    	addConst(elem, expr.realVal(null), expr.unit());
	    else if (expr instanceof BoolConst)
	    	addConst(elem, expr.boolVal(null));
	    else if (expr instanceof IExpr)
	    	addIExpr(elem, (IExpr) expr);
	    else if (expr instanceof VarFuncCall)
	    	addVarFunc(elem, (VarFuncCall) expr);
	    else if (expr instanceof XFuncCall)
	    	addXFunc(elem, (XFuncCall) expr);
	    else if (expr instanceof Summation) {
	        Summation s = (Summation) expr;
	    	addIntegralSum(elem, "sum", s.t(), s.min(), s.max(), s.u());
	    } else if (expr instanceof Integral) {
	        Integral in = (Integral) expr;
	    	addIntegralSum(elem, "int", in.t(), in.min(), in.max(), in.u());
	    } else
	    	addBogus(elem, expr);
	}

	// add Named expr, may contain derivs
	private void addNamed(Element elem, NamedExpr expr) throws Xcept {
	    StringTokenizer stok = new StringTokenizer(
	        expr.toString(), ":");
	    int nbvars = stok.countTokens() - 1;
	    if (nbvars < 1) {
	    	addNamedVar(elem, expr.toString());
		return;
	    }
	    String name = stok.nextToken();

	    // tally bnames & degrees
	    StringList bnames = new StringList();
	    int degreeTot = 0;
	    int[] degrees = new int[nbvars];
	    for (int i=0; i<nbvars; i++) {
	    	String bname = stok.nextToken();
		bnames.addUniq(bname);
		int j = bnames.indexOf(bname);
		degrees[j]++;
		degreeTot++;
	    }
	    
	    // write out deriv
	    Element apply = doc.createElement("apply");
	    int ndim = (expr instanceof Var) ?
	    	((Var) expr).ndim() : 1;
	    String dname = (ndim>1) ? "partialdiff" : "diff";
	    Element diff = doc.createElement(dname);
	    apply.appendChild(diff);
// line below follows spec,  but bolyxes Mozilla viewer: 12 Dec 2006 
//	    if (degreeTot>1) addDegree(apply, degreeTot);
	    for (int i=0; i<bnames.size(); i++) {
	    	if (degrees[i] == 0) continue;
		addBVar(apply, bnames.str(i), degrees[i]);
	    }
	    addNamedVar(apply, name);
	    elem.appendChild(apply);
	}

	// add bvar
	private void addBVar(Element elem, String name, int degree)
	throws Xcept {
	    Element bvar = doc.createElement("bvar");
	    addNamedVar(bvar, name);
	    if (degree > 1) addDegree(bvar, degree);
	    elem.appendChild(bvar);
	}    
	    
	// add degree
	private void addDegree(Element elem, int degree) 
	throws Xcept {
	    Element deg = doc.createElement("degree");
	    addConst(deg, degree, null);
	    elem.appendChild(deg);
	}
	// add Named, no derivs
	private void addNamedVar(Element elem, String name) throws Xcept {
	    Element e = doc.createElement("ci");
	    Text text = doc.createTextNode(name);
	    e.appendChild(text);
	    elem.appendChild(e);
	}

	// add real Const expr
	private void addConst(Element elem, double value, Unit u) 
	throws Xcept {
	    if (value == Expr.pi.realVal(null)) {
	        Element pi = doc.createElement("pi");
		elem.appendChild(pi);
	    } else {	    	
	    	Element e = doc.createElement("cn");
	    	Text text = doc.createTextNode(Util.pretty(value));
		if (u != null) 
		    e.setAttribute("xmml:units", u.name());
	    	e.appendChild(text);
	    	elem.appendChild(e);
	    }
	}

	// add boolean const expr
	private void addConst(Element elem, boolean value)
	throws Xcept {
	    String n = value ? "true" : "false";
	    Element e = doc.createElement(n);
	    elem.appendChild(e);
	}

	// add bogus expr
	private void addIExpr(Element elem, IExpr expr) throws Xcept {
	    int op = expr.op();
	    if (op == IExpr.IF) {
	    	addIf(elem, expr);
		return;
	    }
	    String name = names[op];
	    switch(op) {
	    case IExpr.BESSELI0:
	    case IExpr.BESSELI1:
	    case IExpr.ERF:
	    case IExpr.ERFC:
	    case IExpr.BESSELJN:
	    case IExpr.BESSELKN:
	    case IExpr.BESSELYN:
	        Expr.List args = new Expr.List(expr.nargs());
	        for (int i=0; i<expr.nargs(); i++)
	            args.add(expr.arg(i));
	        addFunc(elem, name, args);
	    	return;
	    }
	    Expr[] args = new Expr[expr.nargs()];
	    Element apply = doc.createElement("apply");
	    for (int i=0; i<args.length; i++) 
	    	args[i] = expr.arg(i);
	    switch(op) {
	    case IExpr.ATAN:
		args = new Expr[] { args[1].div(args[0]) }; 
		break;
	    case IExpr.ROUND:
	    	name = "floor";
		args[0] = args[0].add(Expr.cons(0.5));
		break;
	    }
	    // create name and args
	    if (name == null) name = "op_" + op;
	    Element e = doc.createElement(name);
	    apply.appendChild(e);
	    add(apply, args);
	    elem.appendChild(apply);
	}

	// if-else translates to piecewise
	private void addIf(Element elem, IExpr expr) throws Xcept {
	    Expr a = expr.arg(0);
	    Expr b = expr.arg(1);
	    Expr c = expr.arg(2);
	    Element pwise = doc.createElement("piecewise");
	    Element piece = doc.createElement("piece");
	    add(piece, b);
	    add(piece, a);
	    pwise.appendChild(piece);
	    Element other = doc.createElement("otherwise");
	    add(other, c);
	    pwise.appendChild(other);
	    elem.appendChild(pwise);
	}

	// variable func call
	private void addVarFunc(Element elem, VarFuncCall fcall) 
	throws Xcept {
	    addFunc(elem, fcall.v.toString(), fcall.args);
	}
	
	// XFuncCall
	private void addXFunc(Element elem, XFuncCall fcall) 
	throws Xcept {
	    String name = fcall.func().name();
	    XFuncArg.List xargs = fcall.args();
	    Expr.List args = new Expr.List(xargs.size());
	    for (int i=0; i<xargs.size(); i++)
	    	args.add(xargs.arg(i).base());
	    addFunc(elem, name, args);
	}
	
	// general func call
	private void addFunc(Element elem, String f, Expr.List args) 
	throws Xcept {
	    Element apply = doc.createElement("apply");
	    Element felem = doc.createElement("ci");
	    felem.setAttribute("type", "function");
	    Text ftext = doc.createTextNode(f);
	    felem.appendChild(ftext);
	    apply.appendChild(felem);
	    for (int i=0; i<args.size(); i++) 
	    	add(apply, args.expr(i));
	    elem.appendChild(apply);
	}

	// integral/sum
	private void addIntegralSum(Element elem, String type,
	Expr x, Expr xlo, Expr xhi, Expr expr) throws Xcept {
	    Element apply = doc.createElement("apply");
	    Element integral = doc.createElement(type);
	    apply.appendChild(integral);
	    addBVar(apply, x.toString(), 1);
	    Element lolim = doc.createElement("lowlimit");
	    add(lolim, xlo);
	    apply.appendChild(lolim);
	    Element hilim = doc.createElement("uplimit");
	    add(hilim, xhi);
	    apply.appendChild(hilim);
	    add(apply, expr);	    
	    elem.appendChild(apply);
	}

	// add bogus expr
	private void addBogus(Element elem, Expr expr) throws Xcept {
	    Element e = doc.createElement("bogus");
	    e.setAttribute("str", expr.toString());
	    e.setAttribute("class", expr.getClass().getName());
	    elem.appendChild(e);
	}

	// add Expr array
	private void add(Element elem, Expr[] exprs) throws Xcept {
	    for (int i=0; i<exprs.length; i++)
	    	add(elem, exprs[i]);
	}

	// IExpr name table
	private static String[] names;
	private static void initNames() {
	    if (names != null) return;
	    names = new String[IExpr.OPMAX];
	    names[IExpr.EQ] = "eq";
	    names[IExpr.NE] = "neq";
	    names[IExpr.GT] = "gt";
	    names[IExpr.LT] = "lt";
	    names[IExpr.GE] = "geq";
	    names[IExpr.LE] = "leq";
	    names[IExpr.APPROX] = "approx";
	    names[IExpr.ADD] = "plus";
	    names[IExpr.SUB] = "minus";
	    names[IExpr.MULT] = "times";
	    names[IExpr.DIV] = "divide";
	    names[IExpr.POW] = "power";
	    names[IExpr.REM] = "rem";
	    names[IExpr.ABS] = "abs";
	    names[IExpr.EXP] = "exp";
	    names[IExpr.LN] = "ln";
	    names[IExpr.LOG] = "log";
	    names[IExpr.FLOOR] = "floor";
	    names[IExpr.CEIL] = "ceiling";
	    names[IExpr.AND] = "and";
	    names[IExpr.OR] = "or";
	    names[IExpr.NOT] = "not";
	    names[IExpr.SIN] = "sin";
	    names[IExpr.COS] = "cos";
	    names[IExpr.TAN] = "tan";
	    names[IExpr.ASIN] = "arcsin";
	    names[IExpr.ACOS] = "arccos";
	    names[IExpr.ATAN] = "arctan";
	    names[IExpr.SINH] = "sinh";
	    names[IExpr.COSH] = "cosh";
	    names[IExpr.TANH] = "tanh";
	    names[IExpr.ASINH] = "arcsinh";
	    names[IExpr.ACOSH] = "arccosh";
	    names[IExpr.ATANH] = "arctanh";
	    names[IExpr.SQRT] = "root";
	    names[IExpr.BESSELI0] = "besseli0";
	    names[IExpr.BESSELI1] = "besseli1";
	    names[IExpr.BESSELJN] = "besseljn";
	    names[IExpr.BESSELKN] = "besselkn";
	    names[IExpr.BESSELYN] = "besselyn";
	    names[IExpr.ERF] = "erf";
	    names[IExpr.ERFC] = "erfc";
	}

}
