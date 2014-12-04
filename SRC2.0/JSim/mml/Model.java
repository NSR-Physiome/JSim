/*NSRCOPYRIGHT
	Copyright (C) 1999-2011 University of Washington
	Developed by the National Simulation Resource
	Department of Bioengineering,  Box 355061
	University of Washington, Seattle, WA 98195-5061.
	Dr. J. B. Bassingthwaighte, Director
END_NSRCOPYRIGHT*/

// model

package JSim.mml; import JSim.util.*;
import JSim.expr.*;

import java.io.*;
import java.util.ArrayList;

public class Model extends MathSys implements NameSpace {
	public final static int NONE = 0;
	public final static int OFF = 1;
	public final static int ON = 2;

	public int unitControl; // see above
	public UnitNList units; // units
	public CompProp.List defProps; // default var properties
	protected XFunc.NList xfuncs; // function definitions
	protected Expr.List funcCalls; // function calls
	protected Templ.NList templ; // template definitions
	protected JavaTempl realTempl; // real template
	protected JavaTempl realDomainTempl; // real domain template
	protected JavaTempl intTempl; // int template
	protected JavaTempl realStateTempl; // realState template
	protected JavaTempl intStateTempl; // intState template
	protected JavaTempl choiceTempl; // choice template
	public StringList externJava; // extern java code

	protected ClassLoader classLoader; // for ClassFunc's
	protected String importPath; // for import stmts
	protected Translator translator; // for antimony inclusion
	private Model flatModel;  // flattened version of this
	private MathSys flatMath; // flattened math, for flat model
	private int nscratch; // # scratch vars so far created

	// create new model
	public Model(ClassLoader cload, String ipath) throws Xcept {
	    super(null, "top", null);
	    classLoader = cload;
	    importPath = ipath;

	    unitControl = NONE;
	    units = new UnitNList();
	    defProps = new CompProp.List(2);
	    defProps.add(new CompProp(this, "desc", Expr.STRING));
	    defProps.add(new CompProp(this, "help", Expr.STRING));
	    xfuncs = new XFunc.NList(8);
	    funcCalls = new Expr.List(8);
	    templ = new Templ.NList(8);
	    externJava = new StringList(4);

	    // built-in templates
	    new JavaTempl(this, "math", "JSim.mml.MathSys");
	    realTempl = new JavaTempl(this, "real", "JSim.mml.RealNVar");
	    intTempl = new JavaTempl(this, "int",  "JSim.mml.IntNVar");
	    realStateTempl = new JavaTempl(this, "realState", "JSim.mml.RealStateVar");
	    intStateTempl = new JavaTempl(this, "intState",  "JSim.mml.IntStateVar");
	    choiceTempl = new JavaTempl(this, "choice",  "JSim.mml.ChoiceNVar");
	    realDomainTempl = new JavaTempl(this, "realDomain", "JSim.mml.Domain");
	    XFunc xfunc = new ClassFunc("sum", Expr.REAL,
	    	"JSim.jruntime.RTXSum", classLoader);
	    xfunc.setReentrant(true);
	    xfuncs.add(xfunc);
	    xfunc = new ClassFunc("integral", Expr.REAL, 
	    	"JSim.jruntime.RTXIntegral", classLoader);
	    xfunc.setReentrant(true);
	    xfuncs.add(xfunc);
	}

	// set Translator
	public void setTranslator(Translator t) {
	    translator = t;
	}

	// add function
	public void registerXFunc(XFunc f) throws Xcept {
	    XFunc f1 = xfuncs.xfunc(f.name());
	    if (f1 == null) {
		xfuncs.add(f);
		return;
	    }
	    if (! f.sameAs(f1)) throw new Xcept(f,
		"conflicting function definitions");
	}

	// register template
	public void registerTempl(Templ t) throws Xcept {
	    if (! templ.add(t)) throw new Xcept(t,
		"duplicate template name");
	}

	// get template
	public Templ getTempl(String n) throws Xcept {
	    Templ t = templ.templ(n);
	    if (t == null) throw new Xcept(this,
		"Template " + n + " not found");
	    return t;
	}

	// get special templates
	public JavaTempl realTempl() { return realTempl; }
	public JavaTempl intTempl() { return intTempl; }
	public JavaTempl realStateTempl() { return realStateTempl; }
	public JavaTempl intStateTempl() { return intStateTempl; }
	public JavaTempl choiceTempl() { return choiceTempl; }
	public JavaTempl realDomainTempl() { return realDomainTempl; }

	// dump model
	public void dump(PrintStream out) throws Xcept {
	    out.println("============= DUMPING MODEL UNITS");
	    units.dump(out);
	    out.println("============= DUMPING MODEL TEMPLATES");
	    for (int i=0; i<templ.size(); i++) 
		templ.templ(i).dump(out, "  ");
	    out.println("============= DUMPING MODEL COMPS");
	    super.dump(out, "  ");
	}

	// throw Xcept if model not compatible with sandbox
	public void checkSandbox() throws Xcept {
	    
	    // non-Java source functions incompatible
	    for (int i=0; i<xfuncs.size(); i++) {
		XFunc xfunc = xfuncs.xfunc(i);
		if (xfunc.funcType() == XFunc.SOURCE
		&& xfunc.lang() != XFunc.JAVA)
		    throw new Xcept(xfunc,
			"Non-Java source functions not allowed in JSim sandbox");
	    }
	}

	// flatten this model using temporary file space
	public void flatten(String flatname) throws Xcept {
	    if (flatModel != null || flatMath != null) return;
	    Model mod = this;
	    Sys sys = null;
	    int ct = 0;
	    while (true) {
		if (++ct > 2) throw new Xcept(this,
		    "Overly recursive model flattening.  Probably BCL bug.");
	    	if (mod.child.size() == 0) throw new Xcept(mod,
		    "No components in model");
	    	if (mod.child.size() > 1) throw new Xcept(mod,
		    "Multiple top-level model components not yet supported");
	    	sys = (Sys) mod.child.comp(0);
		if (sys.isFlattenable()) break;
		if (flatname == null) throw new Xcept(mod,
		    "Flattening requires flatname work file");
		Util.verbose("Flattening system " + sys.name + " (pass " + ct + 
		    ") to file " + flatname);
		try {
		    FileOutputStream f1 = new FileOutputStream(flatname);
		    PrintStream out = new PrintStream(f1,true);
		    mod.writeFlat(out);
		    if (out != System.out) out.close();
		} catch (FileNotFoundException e) {
		    throw new Xcept("IO error for flat file " + flatname);
		} 
		mod = new ModelReader(
		   new File(flatname), classLoader, importPath);
		units.merge(mod.units); // import mod units

	    }
	    flatModel = mod;
	    flatMath = (MathSys) sys;
	    flatMath.setFlat();
	    flatMath.expandChildDerivs();
	    if (unitControl == ON) {
		new UnitAssignor(flatMath);
		flatModel.unitCorrectAll();
	    }
	    flatMath.simplifyAll();
	}

	// get flattened version
	public Model getFlatModel() throws Xcept {
	    if (flatModel == null) throw new Xcept(this,
		"Model has not been successfully flattened");
	    return flatModel;
	}
	public MathSys getFlatMath() throws Xcept {
	    if (flatMath == null) throw new Xcept(this,
		"Model has not been successfully flattened");
	    return flatMath;
	}

	// write Flat math version
	public void writeFlat(PrintStream out) throws Xcept {
	    if (child.size() != 1) throw new Xcept(this,
		"Model must contain exactly 1 top-level component");
	    Sys top = (Sys) child.comp(0);

	    // model header and fundamental unit control
	    out.println("JSim v1.1");
	    out.println("");
	    if (unitControl==OFF)
		out.println("unit conversion off;");
	    if (unitControl==ON)
		out.println("unit conversion on;");
	    units.writeFlat(out);

	    // non-default properties (0=desc, 1=help)
	    for (int i=2; i<defProps.size(); i++) {
	    	CompProp p = defProps.prop(i);
		out.println("property " + p.name() + "=string;");
	    }

	    // model specific unit control
	    top.assignUnits();

/*
	    write selected units ???
	    for (int i=0; i<units.fund.size(); i++) 
		out.println("unit " + units.fund.str(i) + " = fundamental;");
	    out.println("");
	    units.writeFlat(out, units);
	    out.println("");
*/	

	    // write functions
	    for (int i=0; i<xfuncs.size(); i++) {
	    	XFunc xfunc = xfuncs.xfunc(i);
		String n = xfunc.name();
		// sum/integral to be phased out,  hack flat for now
		if (! (n.equals("sum") || n.equals("integral")))
		    xfunc.writeFlat(out);
	    }

	    // write flattened top component
	    top.ctxt.unitConst = true;
	    top.writeFlat(out, top.ctxt);
	    top.ctxt.unitConst = false;
	}   

	// get Comp from name
	public Expr compByName(Comp comp, String n) throws Xcept {
	    NamedExpr e = comp.getNamed(n);
	    if (e != null) return e;
	    throw new Xcept(this,  n + " unknown");
	}
	public Expr compByName(String n) throws Xcept {
	    return compByName(this, n);
	}

	// make deriv expression
	public Expr makeDeriv(Expr e1, Expr e2) throws Xcept {
	    return new RealBExpr(IExpr.DERIV, e1, e2);
	}

	// get Unit from name
	public Unit unitByName(String n) throws Xcept {
	    return units.byName(n);
	}

	// function Call from name & args
	public Expr funcCall(String n, Expr.List elist) throws Xcept {
	    return funcCall(this, n, elist);
	}
	public Expr funcCall(Comp comp, String n, Expr.List elist) throws Xcept {

	    // first check variable names for VarFuncCall
	    NamedExpr c = comp.getNamed(n);
	    if (c != null && c instanceof Var) 
		return funcCall(comp, (Var) c, elist);

	    // second check defined functions for XFuncCall
	    XFunc xfunc = xfuncs.xfunc(n);
	    if (xfunc != null) {
		XFuncCall fc = xfunc.createCall(elist);
		fc.setSeq(xfunc.nextSeq());
		funcCalls.add(fc);
		return fc;
	    }

	    // third check built-in functions
	    return IExpr.create(n, elist);
	}		
	public Expr funcCall(Comp comp, Var v, Expr.List elist) throws Xcept {
	    if (v.isDomain() || elist.size() != v.ndim()) 
		throw new Xcept(v, "variable function requires " + 
		    v.ndim() + " arguments");
	    Expr.List domlist = v.domainList();
	    if (domlist.sameAs(elist))
		return v;
	    return new VarFuncCall(v, elist);
	}

	// new scratch variable
	public int newScratch() { return nscratch++; }

	// query funcs
	public XFunc.NList neededFuncs() {
	    XFunc.NList nfuncs = new XFunc.NList(4);
	    for (int i=0; i<xfuncs.size(); i++) {
		XFunc f = xfuncs.xfunc(i);
		if (f.needed()) nfuncs.add(f);
	    }
	    return nfuncs;
	}
	public Expr.List funcCalls() { return funcCalls; }

	// parse unit String
	public Unit parseUnit(String s) throws Xcept {
	    return Unit.parse(this, "1 " + s);
	}
	    
	// Model translator
	public static interface Translator {
	    public String antimony2MML(String text) throws Xcept;
	}

}

